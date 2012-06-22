module Lreader
( rplogToSessions
) where

import Data.List
import Data.Maybe
import Text.XML.Light

import Timestamp
import Message

getSessions :: String -> [Element]
getSessions = findChildren (unqual "session") . fromJust . find isLog . onlyElems . parseXML
    where isLog el = (qName . elName $ el) == "log"

getMessages :: Element -> [Element]
getMessages session = findChildren (unqual "message") session

getName :: Element -> Maybe String
getName session = findAttr (unqual "id") session

getData :: Element -> String
getData = cdData . getText . head . elContent
    where
      getText (Text t) = t

getProtocol :: Element -> Maybe String
getProtocol mes = case findChild (unqual "protocol") mes of
                    Nothing -> Nothing
                    (Just proto) -> Just . getData $ proto

getMsg :: Element -> String
getMsg = getData . fromJust . findChild (unqual "msg")

getFrom :: Element -> String
getFrom = getData . fromJust . findChild (unqual "from")

getTimestamp :: Element -> Timestamp
getTimestamp mes = Timestamp y mon day h min s
    where
      ts = fromJust . findChild (unqual "timestamp") $ mes
      getInt name = read . getData . fromJust . findChild (unqual name) $ ts
      mon = getInt "month"
      day = getInt "day"
      h = getInt "hour"
      min = getInt "minute"
      yr = findChild (unqual "year") ts
      sec = findChild (unqual "second") ts
      y = case yr of
            Nothing -> Nothing
            (Just el) -> Just . read . getData $ el
      s = case sec of
            Nothing -> Nothing
            (Just el) -> Just . read . getData $ el

readMessage :: Element -> Message
readMessage el = Message (getTimestamp el) (getProtocol el) Nothing (getFrom el) (getMsg el)

readSession :: Element -> Session
readSession el = (unMaybe . getName $ el, map readMessage . getMessages $ el)
    where unMaybe Nothing = ""
          unMaybe (Just s) = s

rplogToSessions :: String -> [Session]
rplogToSessions = map readSession . getSessions