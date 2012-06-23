module XMLUtils where

import Text.XML.Light

stringToCont :: String -> Content
stringToCont str = Text $ CData CDataText str Nothing

intToCont :: Int -> Content
intToCont n = Text $ CData CDataRaw (show n) Nothing

simpleElem :: String -> Content -> Element
simpleElem name cont = Element (unqual name) [] [cont] Nothing

buildElem :: String -> [Attr] -> [Content] -> Element
buildElem name attr cont = Element (unqual name) attr cont Nothing

stringElem :: String -> String -> Element
stringElem name conts = simpleElem name $ stringToCont conts

simpleAttr :: String -> String -> Attr
simpleAttr key = Attr $ unqual key