-- | The casing utilities allow you to specify how Aeson renders and parses
-- the field names in JSON messages. Snake, Camel, and Pascal case are all
-- supported. To include casing modifiers in Aeson, attach options to instances
-- of ToJSON and FromJSON.
--
-- > data Person = Person
-- >        { personFirstName :: Text
-- >        , personLastName  :: Text
-- >        } deriving (Generic)
-- >
-- > instance ToJSON Person where
-- >    toJSON = genericToJSON $ aesonPrefix snakeCase
-- > instance FromJSON Person where
-- >    parseJSON = genericParseJSON $ aesonPrefix snakeCase
--
-- The above code will produce JSON messages like the following...
--
-- > {
-- >    "first_name": "John",
-- >    "last_name": "Doe"
-- > }
module Data.Aeson.Casing
    ( aesonDrop
    , aesonPrefix

    , snakeCase
    , trainCase
    , camelCase
    , pascalCase
    ) where

import           Data.Aeson.Casing.Internal
