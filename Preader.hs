module Preader
( pidginToMessage
)where

-- RELIES ON THE FILENAME INCLUDING THE DATE AS yyyy-mm-dd.*

import Text.XML.Light
import Data.Maybe
import Data.List

import Face
import Timestamp
import Message

type Clocktime = (Int, Int, Int) -- h, min, sec
type Caltime = (Int, Int, Int) -- day, mon, year
--type Msg = (Maybe Face, String)

body :: String -> Element
body = head . findChildren (unqual "body") . head . onlyElems . parseXML

notHeading :: Content -> Bool
notHeading (Elem el) = (qName . elName $ el) /= "h3"
notHeading _ = True


-- SHOULD USE DATA.CHAR
notNewline :: Content -> Bool
notNewline (Text t) = (cdData t) `notElem` ["\n", "\r", "\n\r", "\r\n"]
notNewline _ = True

dropHeadings :: [Content] -> [Content]
dropHeadings = filter notHeading

dropNewlines :: [Content] -> [Content]
dropNewlines = filter notNewline

cleanUp :: [Content] -> [Content]
cleanUp = dropHeadings . dropNewlines

isFont :: Content -> Bool
isFont (Elem el) = (qName . elName $ el) == "font"
isFont _ = False

splitByFonts' :: [Content] -> [Content] -> [[Content]] -> [[Content]]
splitByFonts' [] int acc = reverse ((reverse int):acc)
splitByFonts' (c:cs) int acc = if isFont c then
                                   flush
                               else
                                   goOn
    where
      flush = splitByFonts' cs [c] ((reverse int):acc)
      goOn = splitByFonts' cs (c:int) acc

splitByFonts :: [Content] -> [[Content]]
splitByFonts c = filter notEmpty $ splitByFonts' c [] [[]]
    where notEmpty [] = False
          notEmpty _ = True

-- <font> for color of nick, <font> for size of time </font>, <b> nick </b> </font>, (<span style='color: #RRGGBBl'>?) message text (</span>?) <br />

strSplit' :: Char -> String -> [String] -> [String]
strSplit' _ [] acc = reverse acc
strSplit' sep str acc = let (fst, rest) = span (/= sep) str
                        in strSplit' sep (drop 1 rest) (fst:acc)

strSplit :: Char -> String -> [String]
strSplit sep str = strSplit' sep str []

--((h:m:s AM/PM)
-- hours are an ugly hack due to 12 AM denoting 00:00 in pidgin
-- and 12 PM -- 12:00
readClock :: String -> Clocktime
readClock time = (h, m, s)
    where (t, mod) = span (/= ' ') time
          hmss = strSplit ':' t
          hmsi = map read $ hmss :: [Int]
          modif " AM" = 0
          modif " PM" = 12
          h = case head hmsi of
                12 -> case modif mod of
                        0 -> 0
                        _ -> 12
                _ -> (modif mod) + (head hmsi)
          m = hmsi !! 1
          s = hmsi !! 2

readDate :: String -> Caltime
readDate filename = (d,m,y)
    where (dt, rest) = span (/= '.') filename
          ymds = strSplit '-' dt
          ymdi = map read $ ymds :: [Int]
          y = head ymdi
          m = ymdi !! 1
          d = ymdi !! 2

makeTimestamp' :: Caltime -> Clocktime -> Timestamp
makeTimestamp' (day,mon,y) (h,min,sec) = Timestamp (Just y) mon day h min (Just sec)

makeTimestamp :: String -> String -> Timestamp
makeTimestamp filename time = makeTimestamp' (readDate filename) (readClock time)

-- using head due to the way elements are split (i.e. <font> is first)
-- assuming sender's name is the first thing wrapped in <b/> tags
getFrom :: [Content] -> String
getFrom = cleanCol . getText . head .getCont . fromJust . find isB . getCont . head
    where getCont (Elem el) = elContent el
          isB (Elem el) = (qName . elName $ el) == "b"
          isB _ = False
          getText (Text cdat) = cdData cdat
          cleanCol = reverse . drop 1 . reverse -- drop the trailing ':'

-- similar to getFrom (generalize?)
-- looks for <font> within <font>
getStamp :: [Content] -> String
getStamp = cleanPar . getText . head .getCont . fromJust . find isFont . getCont . head
    where getCont (Elem el) = elContent el
          getText (Text cdat) = cdData cdat
          cleanPar = reverse . drop 1 . reverse . drop 1 -- drop the surrounding '()'

getTimestamp :: String -> [Content] -> Timestamp
getTimestamp filename = makeTimestamp filename . getStamp
        
-- drop the leading <font> and trailing <br/>
relevantPart :: [Content] -> [Content]
relevantPart = tail . init

-- convert what's left back to strings
-- leaving <span> color tags as is, for now!
prepareMsg :: [Content] -> [String]
prepareMsg = map showContent . relevantPart

unsafeMsg :: [Content] -> String
unsafeMsg = drop 1 . concat . prepareMsg -- dropping leading space

contentToMessage :: String -> [Content] -> Message
contentToMessage filename cnt = Message ts Nothing Nothing frm mess
    where
      ts = getTimestamp filename cnt
      frm = getFrom cnt
      mess = unsafeMsg cnt

pidginToMessage :: String -> String -> [Message]
pidginToMessage filename xml = map (contentToMessage filename) . splitByFonts . cleanUp . elContent . body $ xml