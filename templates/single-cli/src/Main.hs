{-# LANGUAGE DataKinds #-}

module Main where

import Runix.Runners.CLI.Single (singleTaskRunner)
import __TaskName__ (__taskFunction__)

main :: IO ()
main = singleTaskRunner __taskFunction__