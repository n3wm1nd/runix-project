module Main (main) where

import Test.Hspec
import SSESpec (spec)

main :: IO ()
main = hspec spec
