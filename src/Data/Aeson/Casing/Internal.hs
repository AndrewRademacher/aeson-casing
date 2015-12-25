module Data.Aeson.Casing.Internal where

import           Data.Aeson.Types
import           Data.Char

-- | Creates an Aeson options object that drops a specific number of characters
-- from the front of a field name, then applies a casing function.
aesonDrop :: Int -> (String -> String) -> Options
aesonDrop n f = defaultOptions
        { fieldLabelModifier = f . drop n }

-- | Creates an Aeson options object that drops the field name prefix from a
-- field, then applies a casing function. We assume a convention of the prefix
-- always being lower case, and the first letter of the actual field name being
-- uppercase. This accommodated for field names in GHC 7.8 and below.
--
-- > data Person = Person
-- >        { personFirstName :: Text
-- >        , personLastName  :: Text
-- >        } deriving (Generic)
-- >
-- > data Dog = Dog
-- >        { dogFirstName :: Text
-- >        } deriving (Generic)
--
-- In the above cases, dog and person are always dropped from the JSON field
-- names.
aesonPrefix :: (String -> String) -> Options
aesonPrefix f = defaultOptions
        { fieldLabelModifier = f . dropFPrefix }

----

-- | Snake casing, where the words are always lower case and separated by an
-- underscore.
snakeCase :: String -> String
snakeCase = u . applyFirst toLower
    where u []                 = []
          u (x:xs) | isUpper x = '_' : toLower x : snakeCase xs
                   | otherwise = x : u xs

-- | Camel casing, where the words are separated by the first letter of each
-- word being a capital. However, the first letter of the field is never a
-- capital.
camelCase :: String -> String
camelCase = applyFirst toLower

-- | Pascal casing, where the words are separated by the first letter of each
-- word being a capital. The first letter of the field is always a capital.
pascalCase :: String -> String
pascalCase = applyFirst toUpper

----

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ []     = []
applyFirst f [x]    = [f x]
applyFirst f (x:xs) = f x: xs

dropFPrefix :: String -> String
dropFPrefix []                 = []
dropFPrefix (x:xs) | isUpper x = x : xs
                   | otherwise = dropFPrefix xs

dropCPrefix :: String -> String
dropCPrefix [] = []
dropCPrefix [x] = [x]
dropCPrefix (x0:x1:xs) | isLower x1 = x0 : x1 : xs
                       | otherwise  = dropCPrefix (x1 : xs)
