module HtmlWriter
( sessionsToHtml
) where

import Data.List
import Data.Maybe

import Message
import Timestamp

import Text.XML.Light

import XMLUtils

-- The version without passing the colors [(String, String)]
-- around was much cleaner and readable (passing just Session)
-- but did not guarantee the unique color for every nick throughout
-- multiple sessions.

-- Need a way to fix

getName :: Session -> String
getName (name, _) = name

getMessages :: Session -> [Message]
getMessages (_, msgs) = msgs

allNicks :: Session -> [String]
allNicks = map from . getMessages

getUniqueNicks :: [Session] -> [String]
getUniqueNicks = nub . concat . map allNicks

baseColors :: [String]
baseColors = ["#990000", "#330099", "#ffcc00", "#ff99ff", "#ff9933"]

colors :: [Session] -> [(String, String)]
colors s = zip nicks baseColors
    where nicks = getUniqueNicks s


nickTag :: Message -> Element
nickTag ms = stringElem  "b" ((from ms) ++ ": ")

addZeros :: String -> String
addZeros n = case length n of
               2 -> n
               _ -> '0':n

timeString :: Message -> String
timeString ms = '(':((intercalate ":" times)++") ")
    where ts = timestamp ms
          h = addZeros . show . hour $ ts
          m = addZeros . show . minute $ ts
          sc = case second ts of
                 Nothing -> ""
                 (Just s) -> addZeros . show $ s
          times' = [h,m,sc]
          purge = filter (\s -> length s /= 0)
          times = purge times'


fromTag :: [(String, String)] -> Message -> Element
fromTag cols ms = buildElem "font" [color] [time,nick]
    where color = simpleAttr "color" $ col
          col = snd . fromJust . find (\(n, c) -> n == (from ms)) $ cols
          time = stringToCont . timeString $ ms
          nick = Elem . nickTag $ ms

br :: Element
br = Element (unqual "br") [] [] Nothing

messageToContent :: [(String, String)] -> Message -> [Content]
messageToContent cols ms = [fromT, msgT, lineBreak]
    where fromT = Elem . fromTag cols $ ms
          msgT' = msg ms
          msgT = Text $ CData CDataRaw msgT' Nothing
          lineBreak = Elem $ br

sessionToContent :: [(String, String)] -> Session -> [Content]
sessionToContent cols s = concat content'
    where content' = [[hdr],msgsCont]
          msgsCont = concat msgsCont'
          msgsCont' = map (messageToContent cols) . getMessages $ s
          hdr = Elem . simpleElem "h3" . stringToCont . getName $ s

hr :: Element
hr = Element (unqual "hr") [] [] Nothing

sessionsToContent :: [Session] -> [Content]
sessionsToContent ss = concat . intersperse [Elem hr] . map (sessionToContent (colors ss)) $ ss

body :: [Session] -> Element
body = buildElem "body" [] . sessionsToContent

title :: [Session] -> Element
title ss = stringElem "title" text
    where text = "Conversation log: " ++ (intercalate ", " (getUniqueNicks ss))

headT :: [Session] -> Element
headT = simpleElem "head" . Elem . title

html :: [Session] -> Element
html ss = buildElem "html" [] [hd,bod]
    where hd = Elem . headT $ ss
          bod = Elem . body $ ss

sessionsToHtml :: [Session] -> String
sessionsToHtml = ppElement . html