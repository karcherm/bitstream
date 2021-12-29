import qualified Data.Bit.Stream as BitS
import Data.Bit.Stream (BitStream, BitOrder(..), toBits, fromBits, fromBytesLSB, fromBytesLSBpartial, fromBytesMSB, fromBytesMSBpartial, getUpToN, fastCompare, toBitsType)
import Data.Bit.Reverse
import Text.Parsec.Bits

import Test.Hspec
import Text.Parsec.Prim
import Text.Parsec.Error
import Data.Bits
import Data.List (unfoldr)
import Data.Function (on)

bsDropSimple :: Int -> BitStream -> BitStream
bsDropSimple n bs = (iterate BitS.tail bs) !! n

testP :: BitParser () a -> BitStream -> Either Text.Parsec.Error.ParseError a
testP parser = parse parser "inline test data"

main :: IO ()
main = hspec $ do
    describe "BitReverse" $ do
        describe "revbyte" $ do
            it "should reverse 8 bit value" $ do
                revbyte 0x01 `shouldBe` 0x80
                revbyte 0xAA `shouldBe` 0x55
                revbyte 0xC0 `shouldBe` 0x03
                revbyte 0x33 `shouldBe` 0xCC
        describe "revbitslow" $ do
            it "should reverse low-aligned numbers" $ do
                revbitslow 2 0x01 `shouldBe` 0x02
                revbitslow 2 0x02 `shouldBe` 0x01
                revbitslow 7 0x01 `shouldBe` 0x40
                revbitslow 7 0x04 `shouldBe` 0x10
                revbitslow 7 0x3F `shouldBe` 0x7E
            it "should ignore high-order bits" $ do
                revbitslow 2 0x05 `shouldBe` 0x02
                revbitslow 2 0x81 `shouldBe` 0x02
                revbitslow 2 0xFC `shouldBe` 0
            it "should treat 8-bit input like revbyte" $ do
                revbitslow 8 0x01 `shouldBe` 0x80
                revbitslow 8 0xAA `shouldBe` 0x55

        describe "revbitsfromhigh" $ do
            it "should reverse high-aligned numbers" $ do
                revbitsfromhigh 2 0x40 `shouldBe` 0x02
                revbitsfromhigh 2 0x80 `shouldBe` 0x01
                revbitsfromhigh 7 0x02 `shouldBe` 0x40
                revbitsfromhigh 7 0x08 `shouldBe` 0x10
                revbitsfromhigh 7 0xFC `shouldBe` 0x3F
            it "should ignore low-order bits" $ do
                revbitsfromhigh 2 0x50 `shouldBe` 0x02
                revbitsfromhigh 2 0x41 `shouldBe` 0x02
                revbitsfromhigh 2 0x3F `shouldBe` 0
            it "should treat 8-bit input like revbyte" $ do
                revbitsfromhigh 8 0x01 `shouldBe` 0x80
                revbitsfromhigh 8 0xAA `shouldBe` 0x55

        describe "revbitstohigh" $ do
            it "should reverse numbers to be high-aligned" $ do
                revbitstohigh 2 0x02 `shouldBe` 0x40
                revbitstohigh 2 0x01 `shouldBe` 0x80
                revbitstohigh 7 0x40 `shouldBe` 0x02
                revbitstohigh 7 0x10 `shouldBe` 0x08
                revbitstohigh 7 0x3F `shouldBe` 0xFC
            it "should ignore high-order bits" $ do
                revbitstohigh 2 0x0A `shouldBe` 0x40
                revbitstohigh 2 0x82 `shouldBe` 0x40
                revbitstohigh 2 0xFC `shouldBe` 0
            it "should treat 8-bit input like revbyte" $ do
                revbitstohigh 8 0x01 `shouldBe` 0x80
                revbitstohigh 8 0xAA `shouldBe` 0x55


    describe "BitStream" $ do
        describe "fromBits" $ do
            it "should be inverse to toBits" $ do
                toBits (fromBits []) `shouldBe` []
                toBits (fromBits [True]) `shouldBe` [True]
                toBits (fromBits [True, False]) `shouldBe` [True, False]
                let listlen8 = True : replicate 7 False
                toBits (fromBits listlen8) `shouldBe` listlen8
                let listlen9 = True : listlen8
                toBits (fromBits listlen9) `shouldBe` listlen9
        describe "fromBytes..." $ do
            -- prop "should unfold to the bit list" $
            --     \x -> bitstreamToBits (fromBits x) `shouldBe` x
            it "should understand MSBitFirst byte streams" $ do
                toBits (fromBytesMSB [0xA4,0x00]) `shouldBe`
                  [True, False, True, False,   False, True, False, False] ++ replicate 8 False
                toBits (fromBytesMSBpartial [0xA4,0x00] 4) `shouldBe`
                  [True, False, True, False,   False, True, False, False] ++ replicate 4 False
                toBits (fromBytesMSBpartial [0xA4,0x0F] 4) `shouldBe`
                  [True, False, True, False,   False, True, False, False] ++ replicate 4 False
            it "should understand LSBitFirst byte streams" $ do
                toBits (fromBytesLSB [0xA4,0x00]) `shouldBe`
                  [False, False, True, False,   False, True, False, True] ++ replicate 8 False
                toBits (fromBytesLSBpartial [0xA4,0x00] 4) `shouldBe`
                  [False, False, True, False,   False, True, False, True] ++ replicate 4 False
                toBits (fromBytesLSBpartial [0xA4,0xF0] 4) `shouldBe`
                  [False, False, True, False,   False, True, False, True] ++ replicate 4 False
        describe "getUpToN" $ do
            it "should return 5/3 pairs if asked for 5 bits" $ do
                take 5 (unfoldr (getUpToN 5) (fromBytesLSB [0x81,0x4F])) `shouldBe` [(0x01, 5), (0x04, 3), (0x0F, 5), (0x02, 3)]
                take 5 (unfoldr (getUpToN 5) (fromBytesMSB [0x81,0x4F])) `shouldBe` [(0x10, 5), (0x01, 3), (0x09, 5), (0x07, 3)]
        describe "fastCompare" $ do
            it "should consider any kind of empty streams equal" $ do
                fromBytesLSB [] `fastCompare` fromBytesLSB [] `shouldBe` Just True
                fromBytesLSB [] `fastCompare` fromBytesMSB [] `shouldBe` Just True
            it "should consider streams equal, even if they are bit-reversed" $ do
                fromBytesLSB [0x5F] `fastCompare` fromBytesMSB [0xFA] `shouldBe` Just True
                fromBytesLSB [0x5F] `fastCompare` fromBytesLSB [0x5F] `shouldBe` Just True
                fromBytesLSB [0x5F] `fastCompare` fromBytesMSB [0x5F] `shouldBe` Just False
        describe "==" $ do
            it "should be able to compare misaligned streams" $ do
                let droppedOne = BitS.tail $ fromBytesLSB [0xAA]
                droppedOne == fromBytesLSBpartial [0x55] 7 `shouldBe` True
                droppedOne == fromBytesLSBpartial [0x15] 7 `shouldBe` False

        describe "splitAt" $ do
            it "should support splitting out the empty stream" $ do
                BitS.splitAt 0 (fromBytesLSB []) `shouldBe` Just (BitS.empty, BitS.empty)
                BitS.splitAt 8 (fromBytesLSB [0xAA]) `shouldBe` Just (fromBytesLSB [0xAA], BitS.empty)
                BitS.splitAt 0 (fromBytesLSB [0xAA]) `shouldBe` Just (BitS.empty, fromBytesLSB [0xAA])
            it "should fail splitting out more than we have" $ do
                BitS.splitAt 4 (fromBytesLSBpartial [0x00] 3) `shouldBe` Nothing
                BitS.splitAt 4 (fromBytesLSBpartial [0x00] 4) `shouldBe`
                    Just (fromBytesLSBpartial [0] 4, BitS.empty)
                BitS.splitAt 4 (BitS.tail $ fromBytesLSBpartial [0x00] 4) `shouldBe` Nothing
                BitS.splitAt 4 (BitS.tail $ fromBytesLSBpartial [0x00] 5) `shouldBe`
                    Just (fromBytesLSBpartial [0] 4, BitS.empty)
                BitS.splitAt 4 (bsDropSimple 6 $ fromBytesLSBpartial [0x00,0x00] 1) `shouldBe` Nothing
                BitS.splitAt 4 (bsDropSimple 6 $ fromBytesLSBpartial [0x00,0x00] 2) `shouldBe`
                    Just (fromBytesLSBpartial [0] 4, BitS.empty)
            it "should split correctly at byte boundaries" $ do
                BitS.splitAt 8 (fromBytesLSB [0xAA, 0x55]) `shouldBe`
                    Just (fromBytesLSB [0xAA], fromBytesLSB [0x55])
                BitS.splitAt 7 (BitS.tail $ fromBytesLSB [0xAA, 0x55]) `shouldBe`
                    Just (fromBytesLSBpartial [0x55] 7, fromBytesLSB [0x55])
                BitS.splitAt 7 (BitS.tail $ fromBytesLSBpartial [0xAA, 0x55] 7) `shouldBe`
                    Just (fromBytesLSBpartial [0x55] 7, fromBytesLSBpartial [0x55] 7)
            it "should split correctly inside the head byte" $ do
                BitS.splitAt 4 (fromBytesLSBpartial [0xAA, 0x55] 4) `shouldBe`
                    Just (fromBytesLSBpartial [0x0A] 4, fromBytesLSB [0x5A])
                BitS.splitAt 3 (BitS.tail $ fromBytesLSBpartial [0xAA, 0x55] 4) `shouldBe`
                    Just (fromBytesLSBpartial [0x05] 3, fromBytesLSB [0x5A])
            it "should split correctly inside the tail byte" $ do
                BitS.splitAt 12 (fromBytesLSB [0xAA, 0x55]) `shouldBe`
                    Just (fromBytesLSBpartial [0x0AA, 0x05] 4, fromBytesLSBpartial [0x05] 4)
                BitS.splitAt 12 (fromBytesLSBpartial [0xAA, 0x55] 7) `shouldBe`
                    Just (fromBytesLSBpartial [0x0AA, 0x05] 4, fromBytesLSBpartial [0x05] 3)
                BitS.splitAt 11 (BitS.tail $ fromBytesLSB [0xAA, 0x55]) `shouldBe`
                    Just (fromBytesLSBpartial [0xD5, 0x02] 3, fromBytesLSBpartial [0x05] 4)
                BitS.splitAt 11 (BitS.tail $ fromBytesLSBpartial [0xAA, 0x55] 7) `shouldBe`
                    Just (fromBytesLSBpartial [0xD5, 0x02] 3, fromBytesLSBpartial [0x05] 3)
            it "should split correctly a mid byte" $ do
                BitS.splitAt 12 (fromBytesLSB [0xAA, 0x55, 0xF6]) `shouldBe`
                    Just (fromBytesLSBpartial [0x0AA, 0x05] 4, fromBytesLSBpartial [0x65, 0x0F] 4)
                BitS.splitAt 12 (fromBytesLSBpartial [0xAA, 0x55, 0x76] 7) `shouldBe`
                    Just (fromBytesLSBpartial [0x0AA, 0x05] 4, fromBytesLSBpartial [0x65, 0x07] 3)
                BitS.splitAt 11 (BitS.tail $ fromBytesLSB [0xAA, 0x55, 0xF6]) `shouldBe`
                    Just (fromBytesLSBpartial [0xD5, 0x02] 3, fromBytesLSBpartial [0x65, 0x0F] 4)
                BitS.splitAt 11 (BitS.tail $ fromBytesLSBpartial [0xAA, 0x55, 0x76] 7) `shouldBe`
                    Just (fromBytesLSBpartial [0xD5, 0x02] 3, fromBytesLSBpartial [0x65, 0x07] 3)

        -- Test for BitS.take / BitS.drop are not very thorough, as it is currently implemented
        -- using BitS.splitAt, which is quite intensely tested.
        describe "take" $ do
            it "should support taking nothing" $ do
                BitS.take 0 BitS.empty `shouldBe` BitS.empty
                BitS.take 0 (fromBytesLSBpartial [0x00] 1) `shouldBe` BitS.empty
                BitS.take 0 (fromBytesLSBpartial [0x00, 0x02] 1) `shouldBe` BitS.empty
            it "should handle the bound to overtaking" $ do
                BitS.take 3 (fromBytesLSBpartial [0x5A] 4) `shouldBe` fromBytesLSBpartial [0x02] 3
                BitS.take 4 (fromBytesLSBpartial [0x5A] 4) `shouldBe` fromBytesLSBpartial [0x0A] 4
                BitS.take 5 (fromBytesLSBpartial [0x5A] 4) `shouldBe` fromBytesLSBpartial [0x0A] 4

        describe "drop" $ do
            it "should support dropping nothing" $ do
                BitS.drop 0 BitS.empty `shouldBe` BitS.empty
                BitS.drop 0 (fromBytesLSBpartial [0x00] 1) `shouldBe` fromBytesLSBpartial [0x00] 1
                BitS.drop 0 (fromBytesLSBpartial [0x00, 0x02] 1) `shouldBe` fromBytesLSBpartial [0x00, 0x02] 1
            it "should handle the bound to overdropping" $ do
                BitS.drop 3 (fromBytesLSBpartial [0x5A] 4) `shouldBe` fromBytesLSBpartial [0x01] 1
                BitS.drop 4 (fromBytesLSBpartial [0x5A] 4) `shouldBe` BitS.empty
                BitS.drop 5 (fromBytesLSBpartial [0x5A] 4) `shouldBe` BitS.empty



        describe "toBitsType" $ do
            it "should return 0 for an empty bitstream" $ do
                toBitsType LsbFirst (fromBytesLSB []) `shouldBe` (0 :: Word)
                toBitsType LsbFirst (BitS.tail $ fromBytesLSBpartial [1] 1) `shouldBe` (0 :: Word)
                toBitsType MsbFirst (fromBytesMSB []) `shouldBe` (0 :: Word)
                toBitsType MsbFirst (BitS.tail $ fromBytesMSBpartial [1] 1) `shouldBe` (0 :: Word)
            it "should deserialize a one-bit bitstream containing 1" $ do
                toBitsType LsbFirst (fromBytesLSBpartial [0x01] 1) `shouldBe` (1 :: Word)
                toBitsType LsbFirst (BitS.tail $ fromBytesLSBpartial [0x02] 2) `shouldBe` (1 :: Word)
                toBitsType MsbFirst (fromBytesLSBpartial [0x01] 1) `shouldBe` (1 :: Word)
                toBitsType MsbFirst (BitS.tail $ fromBytesLSBpartial [0x02] 2) `shouldBe` (1 :: Word)
                toBitsType MsbFirst (fromBytesMSBpartial [0x80] 1) `shouldBe` (1 :: Word)
                toBitsType MsbFirst (BitS.tail $ fromBytesMSBpartial [0x40] 2) `shouldBe` (1 :: Word)
                toBitsType LsbFirst (fromBytesMSBpartial [0x80] 1) `shouldBe` (1 :: Word)
                toBitsType LsbFirst (BitS.tail $ fromBytesMSBpartial [0x40] 2) `shouldBe` (1 :: Word)
            it "should mask stray bits during deserialization" $ do
                toBitsType LsbFirst (fromBytesLSBpartial [0xFF] 1) `shouldBe` (1 :: Word)
                toBitsType LsbFirst (BitS.tail $ fromBytesLSBpartial [0xFF] 2) `shouldBe` (1 :: Word)
                toBitsType MsbFirst (fromBytesLSBpartial [0xFF] 1) `shouldBe` (1 :: Word)
                toBitsType MsbFirst (BitS.tail $ fromBytesLSBpartial [0xFF] 2) `shouldBe` (1 :: Word)
                toBitsType MsbFirst (fromBytesMSBpartial [0xFF] 1) `shouldBe` (1 :: Word)
                toBitsType MsbFirst (BitS.tail $ fromBytesMSBpartial [0xFF] 2) `shouldBe` (1 :: Word)
                toBitsType LsbFirst (fromBytesMSBpartial [0xFF] 1) `shouldBe` (1 :: Word)
                toBitsType LsbFirst (BitS.tail $ fromBytesMSBpartial [0xFF] 2) `shouldBe` (1 :: Word)
            it "should deserialize a four-bit bitstream when no endianness swap is required" $ do
                toBitsType LsbFirst (fromBytesLSBpartial [0xB] 4) `shouldBe` (0xB :: Word)
                toBitsType LsbFirst (BitS.tail $ fromBytesLSBpartial [0xB`shiftL`1] 5) `shouldBe` (0xB :: Word)
                toBitsType MsbFirst (fromBytesMSBpartial [0xB0] 4) `shouldBe` (0xB :: Word)
                toBitsType MsbFirst (BitS.tail $ fromBytesMSBpartial [0xB0`shiftR`1] 5) `shouldBe` (0xB :: Word)
            it "should deserialize a four-bit bitstream when endianness swap is required" $ do
                toBitsType MsbFirst (fromBytesLSBpartial [0xD] 4) `shouldBe` (0xB :: Word)
                toBitsType MsbFirst (BitS.tail $ fromBytesLSBpartial [0xD`shiftL`1] 5) `shouldBe` (0xB :: Word)
                toBitsType LsbFirst (fromBytesMSBpartial [0xD0] 4) `shouldBe` (0xB :: Word)
                toBitsType LsbFirst (BitS.tail $ fromBytesMSBpartial [0xD0`shiftR`1] 5) `shouldBe` (0xB :: Word)
            it "should deserialize an aligned 16-bit-value" $ do
                toBitsType LsbFirst (fromBytesLSB [0x34, 0x12]) `shouldBe` (0x1234 :: Word)
                toBitsType MsbFirst (fromBytesMSB [0x12, 0x34]) `shouldBe` (0x1234 :: Word)
                toBitsType MsbFirst (fromBytesLSB [0x48, 0x2C]) `shouldBe` (0x1234 :: Word)
                toBitsType LsbFirst (fromBytesMSB [0x2C, 0x48]) `shouldBe` (0x1234 :: Word)
            it "should deserialize a misaligned 16-bit-value" $ do
                toBitsType LsbFirst (bsDropSimple 4 $ fromBytesLSBpartial [0x4E, 0x23, 0x71] 4) `shouldBe` (0x1234 :: Word)
                toBitsType MsbFirst (bsDropSimple 4 $ fromBytesMSBpartial [0x71, 0x23, 0x4E] 4) `shouldBe` (0x1234 :: Word)
                toBitsType MsbFirst (bsDropSimple 4 $ fromBytesLSBpartial [0x8E, 0xC4, 0x72] 4) `shouldBe` (0x1234 :: Word)
                toBitsType LsbFirst (bsDropSimple 4 $ fromBytesMSBpartial [0x72, 0xC4, 0x8E] 4) `shouldBe` (0x1234 :: Word)



    describe "number" $ do
        it "should parse LSB-first numbers" $ do
             testP (number 2 LsbFirst) (fromBits [True, False]) `shouldBe` Right 1
             testP (number 2 LsbFirst) (fromBits [True, True]) `shouldBe` Right 3
        it "should parse MSB-first numbers" $ do
             testP (number 2 MsbFirst) (fromBits [True, False]) `shouldBe` Right 2
             testP (number 2 MsbFirst) (fromBits [True, True]) `shouldBe` Right 3

