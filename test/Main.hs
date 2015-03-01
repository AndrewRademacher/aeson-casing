module Main where

import           Test.Tasty

import qualified Data.Aeson.Casing.Test as Casing

main :: IO ()
main = defaultMain $ testGroup "Tests"
        [ Casing.tests
        ]
