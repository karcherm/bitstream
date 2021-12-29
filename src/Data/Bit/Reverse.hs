module Data.Bit.Reverse (revbyte, revbitslow, revbitsfromhigh, revbitstohigh) where

import Data.Bits
import Data.Array.Unboxed
import Data.Word (Word8)

reverseUncached :: Word8 -> Word8
reverseUncached x = foldl1 (.|.)
    [x `shiftL` 7, (x `shiftL` 5) .&. 0x40, (x `shiftL` 3) .&. 0x20, (x `shiftL` 1) .&. 0x10,
     x `shiftR` 7, (x `shiftR` 5) .&. 0x02, (x `shiftR` 3) .&. 0x04, (x `shiftR` 1) .&. 0x08]

revmap :: UArray Word8 Word8
revmap = listArray (0,255) [reverseUncached i | i <- [0..255]]

revbyte = (revmap !)
revbitslow cnt val = revbyte val `shiftR` (8 - cnt)
revbitsfromhigh cnt val = revbyte val .&. (bit cnt - 1)
revbitstohigh cnt val = revbyte val .&. negate (bit (8 - cnt))