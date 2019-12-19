module Main where

import           Environment

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui


