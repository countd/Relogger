module Logger
( Session(..)
, sessionsToXML
, heading
) where

import Text.XML.Light

import Face
import Timestamp
import Message

import XMLUtils

timestampToElem :: Timestamp -> Element
timestampToElem ts = Element (unqual "timestamp") [] contList Nothing
    where
      conv name f = Elem $ simpleElem name $ intToCont $ f ts
      d = conv "day" day
      mo = conv "month" month
      h = conv "hour" hour
      mi = conv "minute" minute
      y = case year ts of
            Nothing -> Elem $ blank_element
            (Just yr) -> Elem $ simpleElem "year" $ intToCont yr
      s = case second ts of
            Nothing -> Elem $ blank_element
            (Just sc) -> Elem $ simpleElem "second" $ intToCont sc
      notEmpty (Elem e) = (qName . elName $ e) /= ""
      purge = filter notEmpty
      contList = purge [d,mo,y,h,mi,s]

protocolToElem :: Maybe String -> Maybe Element
protocolToElem Nothing = Nothing
protocolToElem (Just proto) = Just $ stringElem "protocol" proto

textToElem :: Maybe Face -> Maybe Element
textToElem Nothing = Nothing
textToElem (Just tx) = Just $ Element (unqual "text") [] [f,s,c] Nothing
    where f = Elem $ stringElem "font" $ font tx
          s = Elem $ simpleElem "size" $ intToCont $ size tx
          c = Elem $ stringElem "color" $ color tx

messageElemList :: Message -> [Element]
messageElemList m = purify maybeList
    where maybeList = [Just ts, proto, txt, Just frm, Just mess]
          ts = timestampToElem $ timestamp m
          proto = protocolToElem $ protocol m
          txt = textToElem $ text m
          frm = stringElem "from" $ from m
          mess = stringElem "msg" $ msg m
          cleaner el = case el of
                         Nothing -> False
                         _ -> True
          purge = filter cleaner
          unpack = map (\(Just val) -> val)
          purify = unpack . purge

messageToElem :: Message -> Element
messageToElem msg = Element (unqual "message") [] elements Nothing
    where elements = map Elem $ messageElemList msg

sessionToElem :: Session -> Element
sessionToElem (ident, msgs) = Element (unqual "session") [simpleAttr "id" ident] msgElems Nothing
    where msgElements = map messageToElem msgs
          msgElems = map Elem msgElements

sessionsToXML :: [Session] -> String
sessionsToXML ss = ppElement logs
    where logs = Element (unqual "log") [] (map (Elem . sessionToElem) ss) Nothing

heading :: String
heading = "<?xml version=\"1.0\" ?>\n<!DOCTYPE log SYSTEM \"RPLog.dtd\">\n"