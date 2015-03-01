module Data.Aeson.Casing.Internal where

import           Data.Aeson.Types
import           Data.Char

aesonDrop :: Int -> (String -> String) -> Options
aesonDrop n f = defaultOptions
        { fieldLabelModifier = f . drop n }

aesonPrefix :: (String -> String) -> Options
aesonPrefix f = defaultOptions
        { fieldLabelModifier = f . dropFPrefix }

----

snakeCase :: String -> String
snakeCase = u . applyFirst toLower
    where u []                 = []
          u (x:xs) | isUpper x = '_' : toLower x : snakeCase xs
                   | otherwise = x : u xs

camelCase :: String -> String
camelCase = applyFirst toLower

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
