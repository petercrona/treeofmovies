module Auth(Auth(Auth), (<&&>), (<||>)) where

import Data.Text

newtype Auth = Auth (Text -> Bool)

(<&&>) :: Auth -> Auth -> Auth
Auth a <&&> Auth b = Auth $ \r -> a r && b r

(<||>) :: Auth -> Auth -> Auth
Auth a <||> Auth b = Auth $ \r -> a r || b r
