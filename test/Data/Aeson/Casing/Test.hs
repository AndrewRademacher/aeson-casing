{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Aeson.Casing.Test (tests) where

import           Data.Aeson
import           GHC.Generics
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

import           Data.Aeson.Casing.Internal

tests :: TestTree
tests = $(testGroupGenerator)

case_snake :: Assertion
case_snake = do snakeCase "SampleField" @=? "sample_field"
                snakeCase "sampleField" @=? "sample_field"

case_camel :: Assertion
case_camel = do camelCase "SampleField" @=? "sampleField"
                camelCase "sampleField" @=? "sampleField"

case_pascal :: Assertion
case_pascal = do pascalCase "SampleField" @=? "SampleField"
                 pascalCase "sampleField" @=? "SampleField"

case_train :: Assertion
case_train = do trainCase "SampleField" @=? "sample-field"
                trainCase "sampleField" @=? "sample-field"

case_prefix :: Assertion
case_prefix = dropFPrefix "extraSampleField" @=? "SampleField"

----

data Person = Person
        { personFirstName :: String
        , personLastName  :: String
        } deriving (Eq, Show, Generic)

data Animal = Animal
        { animalFirstName :: String
        , animalBreedName :: String
        } deriving (Eq, Show, Generic)

data Config = Config
        { configECRLogin :: Bool
        } deriving (Eq, Show, Generic)

instance ToJSON Person where
    toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Person where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON Animal where
  toJSON = genericToJSON $ aesonPrefix trainCase
instance FromJSON Animal where
  parseJSON = genericParseJSON $ aesonPrefix trainCase

instance ToJSON Config where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Config where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

johnDoe = Person "John" "Doe"

johnDoeJSON = "{\"first_name\":\"John\",\"last_name\":\"Doe\"}"

persianEgypt = Animal "Toffee" "Persian Cat"

persianEgyptJSON = "{\"breed-name\":\"Persian Cat\",\"first-name\":\"Toffee\"}"

logInto = Config True

logIntoJSON = "{\"e_cr_login\":true}"

case_encode_snake :: Assertion
case_encode_snake = do
        let b = encode johnDoe
        b @=? johnDoeJSON

case_decode_snake :: Assertion
case_decode_snake = do
        let p = decode johnDoeJSON :: Maybe Person
        p @=? Just johnDoe

case_encode_snake2 :: Assertion
case_encode_snake2 = do
        let b = encode logInto
        b @=? logIntoJSON

case_decode_snake2 :: Assertion
case_decode_snake2 = do
        let p = decode logIntoJSON :: Maybe Config
        p @=? Just logInto


case_encode_train :: Assertion
case_encode_train = do
        let b = encode persianEgypt
        b @=? persianEgyptJSON

case_decode_train :: Assertion
case_decode_train = do
        let p = decode persianEgyptJSON :: Maybe Animal
        p @=? Just persianEgypt
