module Message
( Message(..)
) where
import Timestamp
import Face

data Message = Message { timestamp :: Timestamp
                       , protocol :: Maybe String
                       , text :: Maybe Face
                       , from :: String
                       , msg :: String
                       } deriving(Show)