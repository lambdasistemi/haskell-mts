-- |
-- Module      : CSMT.Fifo
-- Description : FIFO queue key encoding for CSMT-backed queues
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Counter-based key encoding for FIFO queues on top of CSMT.
--
-- A FIFO queue uses sequential counter values as CSMT keys.
-- The counter is encoded as a fixed-width big-endian 'ByteString'
-- so that trie ordering matches enqueue order.
--
-- Head\/tail counters are stored externally (on-chain datum for
-- validators, indexer state for off-chain services).
module CSMT.Fifo
    ( counterToKey
    )
where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word8)

-- | Encode a counter as a fixed-width big-endian 'ByteString'.
--
-- @counterToKey indexBytes counter@ produces exactly @indexBytes@ bytes.
-- Silently truncates if @counter >= 2^(8*indexBytes)@, matching Aiken's
-- @from_int_big_endian@ behaviour.
--
-- >>> counterToKey 1 0
-- "\NUL"
-- >>> counterToKey 1 128
-- "\128"
-- >>> counterToKey 2 256
-- "\SOH\NUL"
counterToKey :: Int -> Int -> ByteString
counterToKey n c =
    B.pack
        [ fromIntegral @Int @Word8 $ (c `shiftR` (i * 8)) .&. 0xFF
        | i <- [n - 1, n - 2 .. 0]
        ]
