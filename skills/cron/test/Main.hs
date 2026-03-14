module Main (main) where

import Test.Hspec
import qualified Cron.InterpreterSpec
import qualified Cron.MockedSpec
import qualified Cron.IntegrationSpec

main :: IO ()
main = hspec $ do
  Cron.InterpreterSpec.spec
  Cron.MockedSpec.spec
  Cron.IntegrationSpec.spec
