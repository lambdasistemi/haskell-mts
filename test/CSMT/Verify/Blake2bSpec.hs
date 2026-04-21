-- |
-- Module      : CSMT.Verify.Blake2bSpec
-- Description : Pure-Haskell Blake2b-256 vs crypton cross-check
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- Validates the pure-Haskell Blake2b-256 in 'CSMT.Verify.Blake2b'
-- against @crypton@'s C implementation across a wide range of input
-- sizes. The rest of the csmt-verify cross-check (proof-level) lives
-- in 'CSMT.VerifySpec' and trusts this module as its foundation —
-- if this spec fails, proof verification can still accidentally
-- succeed on inputs that both impls happen to agree on, so this
-- spec is the load-bearing one for WASM reproducibility.
module CSMT.Verify.Blake2bSpec (spec) where

import CSMT.Verify.Blake2b (blake2b256)
import Crypto.Hash (Blake2b_256, Digest, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (arbitrary, choose, forAll, vectorOf, (===))

spec :: Spec
spec = describe "Blake2b" $ do
    prop "pure Blake2b-256 matches crypton on arbitrary input"
        $ forAll
            ( do
                n <- choose (0, 2000)
                B.pack <$> vectorOf n arbitrary
            )
        $ \bs ->
            let ours = blake2b256 bs
                theirs =
                    convert (hash bs :: Digest Blake2b_256)
                        :: ByteString
            in  ours === theirs
