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

case_prefix :: Assertion
case_prefix = dropFPrefix "extraSampleField" @=? "SampleField"

----

data Person = Person
        { personFirstName :: String
        , personLastName  :: String
        } deriving (Eq, Show, Generic)

instance ToJSON Person where
    toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Person where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

case_encode_snake :: Assertion
case_encode_snake = do
        let p = Person "John" "Doe"
            b = encode p
        b @=? "{\"first_name\":\"John\",\"last_name\":\"Doe\"}"

case_decode_snake :: Assertion
case_decode_snake = do
        let b = "{\"first_name\":\"John\",\"last_name\":\"Doe\"}"
            p = decode b :: Maybe Person
        p @=? Just (Person "John" "Doe")
