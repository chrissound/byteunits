module Main where

import Test.QuickCheck
import Data.ByteUnits
import Test.HUnit

hUnitTests = test [
    "" ~: "" ~: getBytes (ByteValue 125 Bytes) ~=? 125
  , "" ~: "" ~: getBytes (ByteValue 1024 Bytes) ~=? 1024
  , "" ~: "" ~: getBytes (ByteValue 2048 KiloBytes) ~=? (2048 * 1024)
  , "" ~: "" ~: getBytes (ByteValue 3.0 MegaBytes) ~=? (3 * 1024 * 1024)
  , "" ~: "" ~: (convertByteUnit (ByteValue (3 * 1024 * 1024) Bytes) MegaBytes) ~=? ByteValue 3.0 MegaBytes
  , "" ~: "" ~: (convertByteUnit (ByteValue (3 * 1024 * 1024) Bytes) MegaBytes) ~=? ByteValue 3.0 MegaBytes
  , "" ~: "" ~: getAppropriateUnits (ByteValue 125 Bytes) ~=? (ByteValue 125 Bytes)
  -- TODO: figure out if Hunit has a not equal assertion
  , "" ~: "" ~: getAppropriateUnits (ByteValue 125 Bytes) /= ByteValue 3.0 MegaBytes ~=? True
  , "" ~: "" ~: getBytes (ByteValue 3 KiloBytes) ~=? (3 * 1024)
  , "" ~: "" ~: convertByteUnit (ByteValue (1024 * 1024) KiloBytes) MegaBytes ~=? ByteValue 1024 MegaBytes
  , "" ~: "" ~: getShortHand (ByteValue 1024 MegaBytes) ~=? "1024.00 MB"
  ]

main = do
  runTestTT hUnitTests
