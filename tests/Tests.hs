module Main where

import Test.QuickCheck (quickCheck)
import ByteUnits

main = do
  quickCheck (getUnits KiloBytes 1024 == 1.0)
  quickCheck (getUnits KiloBytes 2048 == 2.0)
  quickCheck (getUnits MegaBytes (1024 * 1024) == 1.0)
  quickCheck (getUnits MegaBytes (3 * 1024 * 1024) == 3.0)
  quickCheck (getAppropriateUnits (3 * 1024 * 1024) == (MegaBytes, 3.0))
  quickCheck (getAppropriateUnits (125) == (Bytes, 125.0))
  quickCheck (getAppropriateUnits (125) /= (MegaBytes, 3.0))
