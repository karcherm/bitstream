module Text.Parsec.Bits
    ( BitParser,
      BitParserT,
      number,
      numberFull
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


gnumber :: (Monad m, Num a, Bits a) => Int -> BitOrder -> BitParserT u m a
gnumber bitcnt order = do
    toBitsType order <$> bitstream bitcnt

number :: (Monad m) => Int -> BitOrder -> BitParserT u m Word 
number = gnumber

withMonadicResultProxy :: (Proxy a -> BitParserT u m a) -> BitParserT u m a
withMonadicResultProxy action = action undefined

numberFull :: (Monad m, Num a, FiniteBits a) => BitOrder -> BitParserT u m a
numberFull order = withMonadicResultProxy (\prx -> gnumber (finiteBitSize (zeroBits `asProxyTypeOf` prx)) order)
