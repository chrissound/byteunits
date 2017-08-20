module ByteUnits where

import Safe

data ByteUnit = Bytes | KiloBytes | MegaBytes | GigaBytes | TeraBytes | PetaBytes | ExaBytes deriving (Show, Eq)

getUnits :: ByteUnit -> Float -> Float
getUnits bytesUnit bytes = case bytesUnit of
  Bytes -> bytes
  KiloBytes -> bytes / (1024 ** 1)
  MegaBytes -> bytes / (1024 ** 2)
  GigaBytes -> bytes / (1024 ** 3)
  TeraBytes -> bytes / (1024 ** 4)
  PetaBytes -> bytes / (1024 ** 4)
  ExaBytes  -> bytes / (1024 ** 5)

-- | Rounds up to the highest unit provided it's > 1
--
-- >>> getAppropriateUnits 1024
-- (KiloBytes,1.0)
--
-- >>> getAppropriateUnits (3.5 * 1024* 1024)
-- (MegaBytes,3.5)
getAppropriateUnits :: Float -> (ByteUnit, Float)
getAppropriateUnits bytes = do
  let units = fmap (\bu -> (bu, getUnits bu bytes)) [Bytes, KiloBytes, MegaBytes, GigaBytes, TeraBytes, PetaBytes, ExaBytes]
  let appropriateUnits = filter (((<=) 1.0) . snd)  units
  case (lastMay appropriateUnits) of
    Just (x) -> x
    Nothing -> (Bytes, bytes)
