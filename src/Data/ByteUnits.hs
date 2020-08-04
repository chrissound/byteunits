-- | Here is a quick example:
--
--  > ByteValue (1024 * 1024 * 3) Bytes
--  > -- the above will evaluate to: ByteValue 3145728.0 Bytes
--
--  > getShortHand . getAppropriateUnits $ ByteValue (1024 * 1024 * 3) Bytes
--  > -- the above will evaluate to: "3.00 MB"

module Data.ByteUnits where

import Safe (lastMay)
import Numeric


data ByteUnit = Bytes | KiloBytes | MegaBytes | GigaBytes | TeraBytes | PetaBytes | ExaBytes deriving (Show, Eq)

data ByteValue = ByteValue Float ByteUnit deriving (Show, Eq)


-- | Also allows comparing sizes, but because it uses float - it might not be 100% accurate
--
-- >>> ByteValue 1024 MegaBytes == ByteValue 1 GigaBytes
-- False
--
-- >>> ByteValue 1023 MegaBytes < ByteValue 1 GigaBytes
-- True
instance Ord ByteValue where
  compare a b = compare (getBytes a) (getBytes b)

-- | Gets the value of bytes from a ByteValue type
--
getBytes :: ByteValue -> Float
getBytes (ByteValue v bu) = case bu of
  Bytes -> v
  KiloBytes -> v * (1024 ** 1)
  MegaBytes -> v * (1024 ** 2)
  GigaBytes -> v * (1024 ** 3)
  TeraBytes -> v * (1024 ** 4)
  PetaBytes -> v * (1024 ** 5)
  ExaBytes  -> v * (1024 ** 6)

-- | Converts the ByteValue to an ByteValue with the specified ByteUnit
--
-- >>> convertByteUnit (ByteValue 500 GigaBytes) MegaBytes
-- ByteValue 512000.0 MegaBytes
convertByteUnit :: ByteValue -> ByteUnit -> ByteValue
convertByteUnit bv bu = case bu of
  Bytes -> ByteValue bytes Bytes
  KiloBytes -> ByteValue (bytes / (1024 ** 1)) KiloBytes
  MegaBytes -> ByteValue (bytes / (1024 ** 2)) MegaBytes
  GigaBytes -> ByteValue (bytes / (1024 ** 3)) GigaBytes
  TeraBytes -> ByteValue (bytes / (1024 ** 4)) TeraBytes
  PetaBytes -> ByteValue (bytes / (1024 ** 5)) PetaBytes
  ExaBytes  -> ByteValue (bytes / (1024 ** 6)) ExaBytes
  where bytes = getBytes bv

-- | Converts to the largest unit size provided the float value is > 1
--
-- >>> getAppropriateUnits (ByteValue 1024 Bytes)
-- ByteValue 1 KiloBytes
--
-- >>> getAppropriateUnits (ByteValue (3.5 * 1024* 1024) Bytes)
-- ByteValue 3.5 MegaBytes
getAppropriateUnits :: ByteValue -> ByteValue
getAppropriateUnits bv = do
  let bUnits = [Bytes, KiloBytes, MegaBytes, GigaBytes, TeraBytes, PetaBytes, ExaBytes]
  let bytes = getBytes bv
  let units = fmap (\bu -> convertByteUnit (ByteValue bytes Bytes) bu) bUnits
  let appropriateUnits = filter (\(ByteValue v' _) -> (v' >= 1.0)) units
  case (lastMay appropriateUnits) of
    Just (bv') -> bv'
    Nothing -> bv

-- | Converts to a short string representation 
--
-- >>> getShortHand $ ByteValue 100 MegaBytes
-- "100.00 MB"
getShortHand :: ByteValue -> String
getShortHand (ByteValue v bu) = (showFFloat (Just 2) v) (" " ++buShort) where
  buShort = case bu of
    Bytes -> "B"
    KiloBytes -> "KB"
    MegaBytes -> "MB"
    GigaBytes -> "GB"
    TeraBytes -> "TB"
    PetaBytes -> "PB"
    ExaBytes -> "EB"
