module Data.ByteUnits where

import Safe

data ByteUnit = Bytes | KiloBytes | MegaBytes | GigaBytes | TeraBytes | PetaBytes | ExaBytes deriving (Show, Eq)

data ByteValue = ByteValue Float ByteUnit deriving (Show, Eq)

-- | Gets the value of bytes from a ByteValue type
--
getBytes :: ByteValue -> Float
getBytes (ByteValue v bu) = case bu of
  Bytes -> v
  KiloBytes -> v * (1024 ** 1)
  MegaBytes -> v * (1024 ** 2)
  GigaBytes -> v * (1024 ** 3)
  TeraBytes -> v * (1024 ** 4)
  PetaBytes -> v * (1024 ** 4)
  ExaBytes  -> v * (1024 ** 5)

-- | Converts the ByteValue to an ByteValue with the specified ByteUnit
--
convertByteUnit :: ByteValue -> ByteUnit -> ByteValue
convertByteUnit bv bu = case bu of
  Bytes -> ByteValue bytes Bytes
  KiloBytes -> ByteValue (bytes / (1024 ** 1)) KiloBytes
  MegaBytes -> ByteValue (bytes / (1024 ** 2)) MegaBytes
  GigaBytes -> ByteValue (bytes / (1024 ** 3)) GigaBytes
  TeraBytes -> ByteValue (bytes / (1024 ** 4)) TeraBytes
  PetaBytes -> ByteValue (bytes / (1024 ** 4)) PetaBytes
  ExaBytes  -> ByteValue (bytes / (1024 ** 5)) ExaBytes
  where bytes = getBytes bv

-- | Converts to the largest unit size provided the float value is > 1
--
-- >>> getAppropriateUnits 1024
-- (KiloBytes,1.0)
--
-- >>> getAppropriateUnits (3.5 * 1024* 1024)
-- (MegaBytes,3.5)
getAppropriateUnits :: ByteValue -> ByteValue
getAppropriateUnits bv@(ByteValue v bu) = do
  let bUnits = [Bytes, KiloBytes, MegaBytes, GigaBytes, TeraBytes, PetaBytes, ExaBytes]
  let bytes = getBytes bv
  let units = fmap (\bu -> convertByteUnit (ByteValue bytes Bytes) bu) bUnits
  let appropriateUnits = filter (\(ByteValue v' bu') -> (v' >= 1.0)) units
  case (lastMay appropriateUnits) of
    Just (bv') -> bv'
    Nothing -> bv
