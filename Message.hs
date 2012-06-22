module Message
( Message(..)
, Session(..)
) where
import Timestamp
import Face

type Session = (String, [Message])
data Message = Message { timestamp :: Timestamp
                       , protocol :: Maybe String
                       , text :: Maybe Face
                       , from :: String
                       , msg :: String
                       } deriving(Show)