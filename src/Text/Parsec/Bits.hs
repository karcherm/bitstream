module Text.Parsec.Bits
    ( BitParser,
      BitParserT,
      bitstream,
      one,
      ones,
      ganyNumber,
      anyNumber,
      anyNumberFull,
      number
    ) where

import Data.Bit.Stream (BitStream, BitOrder, toBitsType)
import qualified Data.Bit.Stream as BitS

import Data.Bits
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Pos (incSourceColumn)
import Data.Functor.Identity (Identity)
import Data.Proxy (asProxyTypeOf, Proxy)
import Data.Word (Word8)


type BitParserT u m = ParsecT BitStream u m
type BitParser u = BitParserT u Identity

one :: BitParser a Bool
one = tokenPrim
        (show . fromEnum)
        (\p _ _ -> incSourceColumn p 1)
        (\x -> if x then Just x else Nothing)
         <?> "a one bit"

ones :: Int -> BitParser a [Bool]
ones n = count n one <?> ("a block of " ++ (show n) ++ " one bits")

bitstream :: (Monad m) => Int -> BitParserT u m BitStream
bitstream n = do
  strm <- getInput
  case BitS.splitAt n strm of
    Nothing -> fail "unexpected end of input"
    Just (partA, partB) -> do
      pos <- getPosition 
      setPosition $ incSourceColumn pos n
      setInput partB
      return partA

-- generalized anyNumer
ganyNumber :: (Monad m, Num a, Bits a) => Int -> BitOrder -> BitParserT u m a
ganyNumber bitcnt order = do
    toBitsType order <$> bitstream bitcnt

anyNumber :: (Monad m) => Int -> BitOrder -> BitParserT u m Word 
anyNumber = ganyNumber

withMonadicResultProxy :: (Proxy a -> BitParserT u m a) -> BitParserT u m a
withMonadicResultProxy action = action undefined

anyNumberFull :: (Monad m, Num a, FiniteBits a) => BitOrder -> BitParserT u m a
anyNumberFull order = withMonadicResultProxy (\prx -> ganyNumber (finiteBitSize (zeroBits `asProxyTypeOf` prx)) order)

number :: Int -> BitOrder -> Word -> BitParser a Word
number length order expected = flip label ("number " ++ show expected) $ try $ do
    n <- anyNumber length order
    if n == expected then
        return n
    else
        unexpected ("number " ++ show n)
