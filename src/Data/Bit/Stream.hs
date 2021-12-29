{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- These extensions are only required for the Text.Parsec.Prim.Stream instance

module Data.Bit.Stream
    ( BitStream,
      fromBits,
      toBits,
      getUpToN,
      null,
      head,
      tail,
      take,
      drop,
      splitAt,
      empty,
      toBitsType,
      fromBytesMSB,
      fromBytesMSBpartial,
      fromBytesLSB,
      fromBytesLSBpartial,
      fastCompare,
      BitOrder (..),
    ) where

import Prelude hiding (null, head, tail, take, drop, splitAt)
import qualified Prelude as P
import Data.Bit.Reverse

import Data.Bits
import Data.Word
import Data.Functor.Identity
import Data.Function (on)
import Data.List (unfoldr)
import Data.Maybe (fromMaybe, fromJust)
import Text.Parsec.Prim (Stream(..))     -- just for an instance declaration

-- Helper functions

-- https://stackoverflow.com/q/34366364
initLast :: [a] -> ([a], a)
initLast []  = undefined
initLast [x] = ([], x)
initLast (x:xs) = let (xs', y) = initLast xs
                   in (x:xs', y)

fold1EndSpecial :: (c -> a -> c) -> (c -> a -> b) -> c -> [a] -> b
fold1EndSpecial _ _ _ [] = error "fold1EndSpecial on empty list"
fold1EndSpecial _ onLast accum [x] = onLast accum x
fold1EndSpecial onInit onLast accum (x:xs) = fold1EndSpecial onInit onLast (onInit accum x) xs

foldEdgesSpecial :: b -> (a -> b) -> (a -> c) -> (c -> a -> c) -> (c -> a -> b) -> [a] -> b
foldEdgesSpecial onEmpty _ _ _ _ [] = onEmpty
foldEdgesSpecial _ onSingleton _ _ _ [x] = onSingleton x
foldEdgesSpecial _ _ onFirst onMid onLast (x:xs) = fold1EndSpecial onMid onLast (onFirst x) xs

oneBits :: (Num a, Bits a) => Int -> a
oneBits n = bit n - 1

-- actual module

data BitOrder = MsbFirst | LsbFirst deriving (Eq, Show)
data BitOrder' = BitOrder' { kind :: BitOrder, testbit :: Word8, adjust1 :: Word8 -> Word8, adjustN :: Int -> Word8 -> Word8, unadjustN :: Int -> Word8 -> Word8 }

instance Show BitOrder' where
  show o
    | kind o == MsbFirst = "msbitFirst"
    | otherwise          = "lsbitFirst"

msbitFirst :: BitOrder'
msbitFirst = BitOrder' MsbFirst 128 (`shiftL` 1) (flip shiftL) (flip shiftR)
lsbitFirst :: BitOrder'
lsbitFirst = BitOrder' LsbFirst 1 (`shiftR` 1) (flip shiftR) (flip shiftL)


data BitStream = BitStream BitOrder' Int Int [Word8] deriving Show

emptyWithEndianess' :: BitOrder' -> BitStream
emptyWithEndianess' order = BitStream order 0 0 []

empty :: BitStream
empty = emptyWithEndianess' lsbitFirst

toNumberLSB :: (Bits a) => [Bool] -> a
toNumberLSB bits = foldl (.|.) zeroBits  (map (bit . fst) $ filter snd $ zip [0..] bits)


fromBitsHelper :: [Bool] -> ([Word8], Int)
fromBitsHelper [] = ([], 0)
fromBitsHelper bits =
  let (hh, tt) = P.splitAt 8 bits
      num = toNumberLSB hh
    in if not $ P.null tt
      then let (nums, m) = fromBitsHelper tt
            in (num : nums, m)
      else ([num], length hh)

fromBits :: [Bool] -> BitStream
fromBits bits = let (ws, m) = fromBitsHelper bits
                 in BitStream lsbitFirst m 0 ws

toBits :: BitStream -> [Bool]
toBits = unfoldr (runIdentity . uncons)

fromBytesMSBpartial :: [Word8] -> Int -> BitStream
fromBytesMSBpartial strm lastbits = BitStream msbitFirst lastbits 0 strm

fromBytesMSB :: [Word8] -> BitStream
fromBytesMSB = (flip fromBytesMSBpartial) 8

fromBytesLSBpartial :: [Word8] -> Int -> BitStream
fromBytesLSBpartial strm lastbits = BitStream lsbitFirst lastbits 0 strm

fromBytesLSB :: [Word8] -> BitStream
fromBytesLSB = (flip fromBytesLSBpartial) 8

endianess' :: BitStream -> BitOrder'
endianess' (BitStream o _ _ _) = o

endianess :: BitStream -> BitOrder
endianess = kind . endianess'

toMsbitFirst :: BitStream -> BitStream
toMsbitFirst strm@(BitStream o m n ws)
  | kind o == MsbFirst = strm
  | P.null ws          = BitStream msbitFirst m n ws
  | otherwise          = BitStream msbitFirst m n (revbitstohigh (8 - n) (P.head ws) : map revbyte (P.tail ws))

toLsbitFirst :: BitStream -> BitStream
toLsbitFirst strm@(BitStream o m n ws)
  | kind o == LsbFirst = strm
  | P.null ws          = BitStream lsbitFirst m n ws
  | otherwise          = BitStream lsbitFirst m n (revbitsfromhigh (8 - n) (P.head ws) : map revbyte (P.tail ws))

toEndianess :: BitOrder -> BitStream -> BitStream
toEndianess LsbFirst = toLsbitFirst
toEndianess MsbFirst = toMsbitFirst

makeSameEndianess :: (BitStream, BitStream) -> (BitStream, BitStream)
makeSameEndianess pair@(a, b)
 | endianess a == endianess b = pair
 | otherwise                  = (toMsbitFirst a, toMsbitFirst b)

bscase :: (BitOrder' -> a) -> (BitOrder' -> Int -> Int -> Int -> Word8 -> [Word8] -> a) -> BitStream -> a
bscase empty _ (BitStream order _ _ []) = empty order
bscase empty filled (BitStream order m n (w:ws))
  | P.null ws && n >= m = empty order
  | P.null ws           = filled order m (m-n) n w []
  | otherwise           = filled order m (8-n) n w ws

fastCompare :: BitStream -> BitStream -> Maybe Bool
fastCompare s1@(BitStream o1 m1 n1 ws1) s2@(BitStream o2 m2 n2 ws2)
  | P.null ws1 && P.null ws2 = Just True
  | n1 /= n2                 = Nothing 
  | m1 /= m2                 = Just False
  | P.null ws1 /= P.null ws2 = Just False
  -- both streams have at least one byte, both start at the same bit,
  -- and both streams use the same amount of bits in the last byte
  | otherwise = let (BitStream o _ _ ws1', BitStream _ _ _ ws2') = makeSameEndianess (s1, s2)
                 in if m1 == 8 then Just $ ws1' == ws2'
                               else let (i1, l1) = initLast ws1'
                                        (i2, l2) = initLast ws2'
                                     in Just $ i1 == i2 && unadjustN o (8-m1) l1 ==
                                                           unadjustN o (8-m1) l2

slowCompare :: BitStream -> BitStream -> Bool
slowCompare = (==) `on` toBits

instance Eq BitStream where
  a == b = fromMaybe (slowCompare a b) (fastCompare a b)

getUpToN :: Int -> BitStream -> Maybe ((Word8, Int), BitStream)
getUpToN requested = bscase (const Nothing) $
    \order m avail n w ws ->
      let
        byte_eaten = avail <= requested
        cnt' = if byte_eaten then avail else requested
        extracted
             | kind order == MsbFirst = w `shiftR` (8 - cnt')
             | otherwise              = w .&. (oneBits cnt')
        adjusted_w cnt' = adjustN order cnt' w
      in
        if byte_eaten
        then Just ((extracted, avail),     BitStream order m  0                                    ws )
        else Just ((extracted, requested), BitStream order m (n + requested) (adjusted_w requested:ws))

-- using "Maybe" to return in Nothing in case the input is short.
splitAt :: Int -> BitStream -> Maybe (BitStream, BitStream)
splitAt 0 = \x -> Just (empty, x)
splitAt splitpoint = bscase (const Nothing) $
    \order m avail n w ws ->
      let
        (fullbytes, partialbits) = (splitpoint + n) `divMod` 8
        (midsize, lastbytebits)
          | partialbits == 0       = (0, 8)
          | otherwise              = (1, partialbits)
        adjustamount = if fullbytes == 0 then splitpoint else partialbits
        (streamA_, streamB_') = P.splitAt fullbytes (w:ws)
        (splitbyte, streamB_) = P.splitAt midsize streamB_'
        (streamA, streamB) = (streamA_ ++ splitbyte,
                              map (adjustN order adjustamount) splitbyte ++ streamB_ )
        lenA = length streamA
        isOK = not (P.null streamB_) ||
               (m >= lastbytebits && lenA == (fullbytes + midsize))
      in
        if isOK
          then Just (BitStream order lastbytebits n streamA,
                     BitStream order m partialbits streamB)
          else Nothing

take :: Int -> BitStream -> BitStream
take n bs = maybe bs fst $ splitAt n bs

drop :: Int -> BitStream -> BitStream
drop n bs = maybe (emptyWithEndianess' $ endianess' bs) snd $ splitAt n bs

toBitsType' :: (Num a, Bits a) => BitStream -> a
toBitsType' (BitStream o m n ws)
  | kind o == LsbFirst = foldEdgesSpecial
                                  0                                          -- Empty
                                  (\x -> fromIntegral $ oneBits (m-n) .&. x) -- Singleton
                                  (\x -> (fromIntegral x, 8 - n))            -- Init
                                  (\(accum, b) x -> (accum .|. (fromIntegral x `shiftL` b), b + 8))
                                  (\(accum, b) x -> accum .|. ((oneBits m .&. fromIntegral x) `shiftL` b))
                                  ws
  | otherwise          =  foldEdgesSpecial
                                  0                                          -- Empty
                                  (\x -> fromIntegral $ x `shiftR` (n + (8-m)))  -- Singleton
                                  (\x -> fromIntegral $ x `shiftR` n)        -- Init
                                  (\accum x -> accum `shiftL` 8 .|. fromIntegral x)
                                  (\accum x -> accum `shiftL` m .|. fromIntegral x `shiftR` (8 - m))
                                  ws

toBitsType :: (Num a, Bits a) => BitOrder -> BitStream -> a
toBitsType endian strm = toBitsType' (toEndianess endian strm)

null = bscase (const True) (\_ _ _ _ _ _ -> False)
head :: BitStream -> Bool
head = fst . fromJust . runIdentity . uncons

tail :: BitStream -> BitStream
tail = snd . fromJust . runIdentity . uncons

instance (Monad m) => Stream BitStream m Bool where
    uncons = bscase (const $ return Nothing) $
       \order m _ n w ws ->
           let
                 (n', rem) = if n == 7
                             then (0,  ws)
                             else (n+1, (adjust1 order w : ws))
           in return $ Just (w .&. (testbit order) /= 0, BitStream order m n' rem)
