module Face
( Face(..)
) where

data Face = Face { font :: String
                 , color :: String
                 , size :: Int
                 } deriving(Show)