{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds      #-}

module SevenSeg
    ( SevenSeg (..)
    , SevenSegOut
    , sevenSeg
    , SevenSegs (..)
    , SevenSegsOut (..)
    , sevenSegs
    ) where

import           CLaSH.Prelude

data SevenSeg = SevenSeg
    { hex :: Maybe (BitVector 4)
    , dot :: Bit
    } deriving Show

type SevenSegOut = BitVector 8

sevenSeg :: SevenSeg -> SevenSegOut
sevenSeg (SevenSeg h d) = bs ++# complement d
    where
        bs = case h of
            Just 0x0 -> 0b1000000
            Just 0x1 -> 0b1111001
            Just 0x2 -> 0b0100100
            Just 0x3 -> 0b0110000
            Just 0x4 -> 0b0011001
            Just 0x5 -> 0b0010010
            Just 0x6 -> 0b0000010
            Just 0x7 -> 0b1111000
            Just 0x8 -> 0b0000000
            Just 0x9 -> 0b0011000
            Just 0xA -> 0b0001000
            Just 0xB -> 0b0000011
            Just 0xC -> 0b1000110
            Just 0xD -> 0b0100001
            Just 0xE -> 0b0000110
            Just 0xF -> 0b0001110
            _        -> 0b1111111

data SevenSegs = SevenSegs
    { s3    :: SevenSeg
    , s2    :: SevenSeg
    , s1    :: SevenSeg
    , s0    :: SevenSeg
    } deriving Show

data SevenSegsOut = SevenSegsOut
    { sO    :: SevenSegOut
    , sel   :: BitVector 4
    } deriving Show

sevenSegs :: Signal SevenSegs -> Signal SevenSegsOut
sevenSegs = mealy sevenSegsT 0

sevenSegsT :: BitVector 2 -> SevenSegs -> (BitVector 2, SevenSegsOut)
sevenSegsT i s = (i + 1,
    SevenSegsOut
        (sevenSeg $ case i of
            0 -> s0 s
            1 -> s1 s
            2 -> s2 s
            3 -> s3 s
            _ -> SevenSeg Nothing 0)
        (complement $ 1 `shiftL` fromIntegral i))
