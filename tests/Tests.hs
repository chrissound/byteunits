module Main where

import Test.QuickCheck (quickCheck)

main = do
  quickCheck ("" == "")
