module Logger
( Session(..)
, sessionToXML
, heading
) where

import Text.XML.Light

import Face
import Timestamp
import Message

type Session = (String,[Message])

stringToCont :: String -> Content
stringToCont str = Text $ CData CDataText str Nothing

intToCont :: Int -> Content
intToCont n = Text $ CData CDataRaw (show n) Nothing

simpleElem :: String -> Content -> Element
simpleElem name cont = Element (unqual name) [] [cont] Nothing

stringElem :: String -> String -> Element
stringElem name conts = simpleElem name $ stringToCont conts

simpleAttr :: String -> String -> Attr
simpleAttr key = Attr $ unqual key

timestampToElem :: Timestamp -> Element
timestampToElem ts = Element (unqual "timestamp") [] [d,mo,y,h,mi,s] Nothing
    where conv name f = Elem $ simpleElem name $ intToCont $ f ts
          d = conv "day" day
          mo = conv "month" month
          y = conv "year" year
          h = conv "hour" hour
          mi = conv "minute" minute
          s = conv "second" second

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

sessionToXML :: Session -> String
sessionToXML session = ppElement logs
    where logs = Element (unqual "log") [] [Elem $ sessionToElem session] Nothing

heading :: String
heading = "<?xml version=\"1.0\" ?>\n<!DOCTYPE log SYSTEM \"RPLog.dtd\">\n"