{-# LANGUAGE DataKinds #-}

module Main where

import Runix.Runners.CLI.Multi (Task(..), multiTaskRunner)
-- Import your task modules here, e.g.:
-- import qualified TaskOne
-- import qualified TaskTwo

tasks :: [Task]
tasks =
  [ -- Add your tasks here, e.g.:
    -- Task "taskone" TaskOne.runTask
    -- Task "tasktwo" TaskTwo.runTask
  ]

main :: IO ()
main = multiTaskRunner tasks