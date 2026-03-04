{-# LANGUAGE StrictData #-}

module MPF.Backend.Standalone
    ( MPFStandaloneOp
    , MPFStandaloneCF (..)
    , MPFStandalone (..)
    , MPFStandaloneCodecs (..)
    , mkMPFStandaloneOp
    )
where

import Control.Lens (Prism', type (:~:) (..))
import Data.ByteString (ByteString)
import Database.KV.Transaction
    ( GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    )
import MPF.Interface (HexIndirect (..), HexKey)

-- | Column family identifiers for MPF standalone backend
data MPFStandaloneCF
    = MPFStandaloneKV
    | MPFStandaloneMPF
    | MPFStandaloneJournal
    deriving (Show, Eq, Ord)

-- | Operation type for standalone backend
type MPFStandaloneOp = (MPFStandaloneCF, ByteString, Maybe ByteString)

-- | Create a standalone operation
mkMPFStandaloneOp
    :: MPFStandaloneCF -> ByteString -> Maybe ByteString -> MPFStandaloneOp
mkMPFStandaloneOp = (,,)

-- | GADT for type-safe column family selection
data MPFStandalone k v a x where
    MPFStandaloneKVCol :: MPFStandalone k v a (KV k v)
    MPFStandaloneMPFCol :: MPFStandalone k v a (KV HexKey (HexIndirect a))
    MPFStandaloneJournalCol
        :: MPFStandalone k v a (KV ByteString ByteString)

instance GEq (MPFStandalone k v a) where
    geq MPFStandaloneKVCol MPFStandaloneKVCol = Just Refl
    geq MPFStandaloneMPFCol MPFStandaloneMPFCol = Just Refl
    geq MPFStandaloneJournalCol MPFStandaloneJournalCol = Just Refl
    geq _ _ = Nothing

instance GCompare (MPFStandalone k v a) where
    gcompare MPFStandaloneKVCol MPFStandaloneKVCol = GEQ
    gcompare MPFStandaloneKVCol _ = GLT
    gcompare MPFStandaloneMPFCol MPFStandaloneKVCol = GGT
    gcompare MPFStandaloneMPFCol MPFStandaloneMPFCol = GEQ
    gcompare MPFStandaloneMPFCol MPFStandaloneJournalCol = GLT
    gcompare MPFStandaloneJournalCol MPFStandaloneJournalCol = GEQ
    gcompare MPFStandaloneJournalCol _ = GGT

-- | Serialization codecs for the MPF standalone backend
data MPFStandaloneCodecs k v a = MPFStandaloneCodecs
    { mpfKeyCodec :: Prism' ByteString k
    , mpfValueCodec :: Prism' ByteString v
    , mpfNodeCodec :: Prism' ByteString a
    }
