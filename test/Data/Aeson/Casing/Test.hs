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
data Car = Car
        { carFromCarMAX        :: Bool
        , carIsFREAKINGCoolBro :: Bool
        , carIsACoolThing      :: Bool
        , carAB                :: Bool
        } deriving (Eq, Show, Generic)

instance ToJSON Car where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Car where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


data Person = Person
        { personFirstName :: String
        , personLastName  :: String
        } deriving (Eq, Show, Generic)

instance ToJSON Person where
    toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Person where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase


case_encode_snake_upper_case :: Assertion
case_encode_snake_upper_case = do
      let p = Car True True True True
          b = encode p
      b @=? "{\"from_car_max\":true,\"ab\":true,\"is_freaking_cool_bro\":true,\"is_a_cool_thing\":true}"


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
