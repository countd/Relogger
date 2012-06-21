module Treader
( trillianToMessages
) where
import Numeric
import Data.List
import Data.Char
import Text.XML.Light

import Timestamp
import Message

isolateEncoded :: String -> [String]
isolateEncoded = flip isolateEncoded' $ []

isolateEncoded' :: String -> [String] -> [String]
isolateEncoded' "" acc = reverse acc
isolateEncoded' str@(c:cs) acc = case c of
                                   '%' -> isolateEncoded' (drop 3 str) ((take 3 str):acc)
                                   _ -> isolateEncoded' (dropWhile (\ch -> ch /= '%') str) ((takeWhile (\ch -> ch /= '%') str):acc)

hexStringToInteger :: String -> Int
hexStringToInteger = unpack . head . readHex 
    where unpack (x, _) = x

readEncoded :: String -> Char
readEncoded = chr . hexStringToInteger . tail -- tail is used to drop the leading '%'

decodePiece :: String -> String
decodePiece str@(c:cs) = case c of
                           '%' -> [readEncoded str]
                           _ -> str

decodeList :: [String] -> [String]
decodeList = map decodePiece

joinStrings :: [String] -> String
joinStrings = concat

parseString :: String -> String
parseString = joinStrings . decodeList . isolateEncoded

parseEncoded :: Maybe String -> String
parseEncoded x = case x of
                   Nothing -> ""
                   Just str -> parseString str

getElems :: String -> [Element]
getElems = onlyElems . parseXML

getAttr :: String -> Element -> String
getAttr name elem = parseEncoded $ findAttr key elem
    where key = unqual name

parseMessage :: Element -> Message
parseMessage el = Message timestamp protocol text from msg
    where timestamp = stringToTimestamp $ getAttr "time" el
          protocol = Just $ getAttr "medium" el
          text = Nothing
          from = getAttr "from" el
          msg = getAttr "text" el

elementsToMessages :: [Element] -> [Message]
elementsToMessages = map parseMessage

filterMessages :: [Element] -> [Element]
filterMessages = filter (\el -> (qName $ elName el) == "message")

trillianToMessages :: String -> [Message]
trillianToMessages = elementsToMessages . filterMessages . getElems