# bitstream

## Overview
This package implements a wrapper around byte-based streams to access values and substreams in a bitwise fashion. The intended use case is parsing data formats that do not care about byte alignment of their bit streams, like many compressed data formats or formats of data that is not indended for byte-oriented processing, e.g. FPGA bitstreams.

## Parts
It contains of three modules:
- Data.Bit.Reverse is primarily a helper module to mirror the bit order inside a byte, but it has been exposed because the facility it provides seems generally useful
- Data.Bit.Stream contains a wrapper layer that makes Word8 lists accessible like they were bit lists, without needing a list-of-Bool data structure as middleman. Basically, it is a emulation of `[Bool]` based on `[Word8]`
- Text.Parsec.Bits contains a Parsec type alias fixing Data.Bit.Stream.BitStream as token source, and functionality to extract binary numbers from a BitStream parser without needing to go down to the bit level.

## Future ideas
- Change base type for BitStream from `[Word8]` to `Data.ByteString.Lazy`
- Generalize BitStream to support arbitrary Word sizes like `[Word16]` or `[Word32]` as data source
- Implemented mixed bitwise/bytewise access for simple compressed formats like the proprietary ones used by LZEXE, PKLITE or the UCL-based algorithm used in UPX.