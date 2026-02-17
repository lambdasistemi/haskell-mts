{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}

module CSMT.Test.Lib
    ( delete
    , deleteWord64
    , deleteM
    , deleteMWord64
    , genKey
    , genPaths
    , genSomePaths
    , node
    , insert
    , inserted
    , insertWord64
    , insertM
    , insertMWord64
    , intHash
    , word64Hashing
    , mkDeletionPath
    , proofM
    -- , summed
    , verifyM
    , verifyMHash
    , verifyMWord64
    , verifyMList
    , listHashing
    , keyToListOfWord64
    , insertMHash
    , insertMList
    , identityFromKV
    , element
    , list
    , ListOf
    , evalPure
    , evalPureFromEmptyDB
    , indirect
    , runPureFromEmptyDB
    , insertIndirectM
    , insertWord64s
    , insertHashes
    , manyRandomPaths
    , word64Codecs
    , hashCodecs
    , listOfWord64Codecs
    )
where

import CSMT
    ( Direction (L, R)
    , Hashing
    , InclusionProof
    , Indirect (..)
    , Key
    , Standalone (..)
    , StandaloneCodecs (..)
    , buildInclusionProof
    , inserting
    , keyPrism
    , verifyInclusionProof
    )
import CSMT.Backend.Pure
    ( InMemoryDB
    , Pure
    , emptyInMemoryDB
    , pureDatabase
    , runPure
    )
import CSMT.Deletion
    ( DeletionPath (..)
    , deleting
    , newDeletionPath
    )
import CSMT.Hashes (Hash, hashHashing, isoHash, mkHash)
import CSMT.Interface (FromKV (..), Hashing (..))
import Control.Lens (simple)
import Control.Lens (Prism', prism')
import Control.Monad (replicateM)
import Control.Monad.Free (Free (..), liftF)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..), foldl')
import Data.List (nub)
import Data.Serialize
    ( getWord16be
    , getWord64be
    , putWord16be
    , putWord64be
    )
import Data.Serialize.Extra (evalGetM, evalPutM)
import Data.String (IsString (..))
import Data.Word (Word64)
import Database.KV.Transaction (runTransactionUnguarded)
import Test.QuickCheck
    ( listOf
    , listOf1
    , scale
    , shuffle
    )
import Test.QuickCheck.Gen (Gen, elements)

identityFromKV :: FromKV Key a a
identityFromKV = FromKV{isoK = simple, fromV = id, treePrefix = const []}

word64Hashing :: Hashing Word64
word64Hashing =
    Hashing
        { rootHash = \(Indirect k v) -> keyToWord64 k + v
        , combineHash = \(Indirect kl l) (Indirect kr r) ->
            keyToWord64 kl + keyToWord64 kr + l + r
        }

insert
    :: Ord k
    => StandaloneCodecs k v a
    -> FromKV k v a
    -> Hashing a
    -> InMemoryDB
    -> k
    -> v
    -> InMemoryDB
insert codecs fromKV hashing m k v =
    snd $ runPure m $ insertM codecs fromKV hashing k v

delete
    :: Ord k
    => StandaloneCodecs k v a
    -> FromKV k v a
    -> Hashing a
    -> InMemoryDB
    -> k
    -> InMemoryDB
delete codecs fromKV hashing m k =
    snd
        $ runPure m
        $ deleteM codecs fromKV hashing k

word64Codecs :: StandaloneCodecs Key Word64 Word64
word64Codecs =
    StandaloneCodecs
        { keyCodec = keyPrism
        , valueCodec = word64Prism
        , nodeCodec = word64Prism
        }

deleteWord64 :: InMemoryDB -> Key -> InMemoryDB
deleteWord64 = delete word64Codecs identityFromKV word64Hashing

insertWord64
    :: InMemoryDB -> Key -> Word64 -> InMemoryDB
insertWord64 = insert word64Codecs identityFromKV word64Hashing
insertM
    :: Ord k
    => StandaloneCodecs k v a
    -> FromKV k v a
    -> Hashing a
    -> k
    -> v
    -> Pure ()
insertM codecs fromKV hashing k v =
    runTransactionUnguarded (pureDatabase codecs)
        $ inserting fromKV hashing StandaloneKVCol StandaloneCSMTCol k v

word64Prism :: Prism' ByteString Word64
word64Prism = prism' encode decode
  where
    encode :: Word64 -> ByteString
    encode = evalPutM . putWord64be
    decode :: ByteString -> Maybe Word64
    decode = evalGetM getWord64be

deleteM
    :: Ord k
    => StandaloneCodecs k v a
    -> FromKV k v a
    -> Hashing a
    -> k
    -> Pure ()
deleteM codecs fromKV hashing k =
    runTransactionUnguarded (pureDatabase codecs)
        $ deleting fromKV hashing StandaloneKVCol StandaloneCSMTCol k

insertMWord64 :: Key -> Word64 -> Pure ()
insertMWord64 = insertM word64Codecs identityFromKV word64Hashing

hashCodecs :: StandaloneCodecs Key Hash Hash
hashCodecs =
    StandaloneCodecs
        { keyCodec = keyPrism
        , valueCodec = isoHash
        , nodeCodec = isoHash
        }

insertMHash :: Key -> Hash -> Pure ()
insertMHash = insertM hashCodecs identityFromKV hashHashing

insertMList :: Key -> [Word64] -> Pure ()
insertMList = insertM listOfWord64Codecs identityFromKV listHashing

keyToWord64 :: Key -> Word64
keyToWord64 = foldl' (\acc d -> acc * 2 + dirToBit d) 0
  where
    dirToBit L = 0
    dirToBit R = 1

listOfWord64Codecs :: StandaloneCodecs Key [Word64] [Word64]
listOfWord64Codecs =
    StandaloneCodecs
        { keyCodec = keyPrism
        , valueCodec = listOfWord64Prism
        , nodeCodec = listOfWord64Prism
        }

listOfWord64Prism :: Prism' ByteString [Word64]
listOfWord64Prism = prism' encode decode
  where
    encode :: [Word64] -> ByteString
    encode xs = evalPutM $ do
        putWord16be $ fromIntegral (length xs)
        mapM putWord64be xs

    decode :: ByteString -> Maybe [Word64]
    decode = evalGetM $ do
        len <- fromIntegral <$> getWord16be
        replicateM len getWord64be

deleteMWord64 :: Key -> Pure ()
deleteMWord64 = deleteM word64Codecs identityFromKV word64Hashing

proofM
    :: Ord k
    => StandaloneCodecs k v a
    -> FromKV k v a
    -> Hashing a
    -> k
    -> Pure (Maybe (v, InclusionProof a))
proofM codecs fromKV hashing k =
    runTransactionUnguarded (pureDatabase codecs)
        $ buildInclusionProof
            fromKV
            StandaloneKVCol
            StandaloneCSMTCol
            hashing
            k

verifyM
    :: (Eq a, Eq v, Ord k)
    => StandaloneCodecs k v a
    -> FromKV k v a
    -> Hashing a
    -> k
    -> v
    -> Pure Bool
verifyM codecs fromKV hashing k expectedV = do
    mp <- proofM codecs fromKV hashing k
    pure $ case mp of
        Nothing -> False
        Just (v, p) -> v == expectedV && verifyInclusionProof hashing p

verifyMWord64 :: Key -> Word64 -> Pure Bool
verifyMWord64 = verifyM word64Codecs identityFromKV word64Hashing

verifyMList :: Key -> [Word64] -> Pure Bool
verifyMList = verifyM listOfWord64Codecs identityFromKV listHashing

keyToListOfWord64 :: Key -> [Word64]
keyToListOfWord64 xs = flip fmap xs $ \case
    L -> 0
    R -> 1

listHashing :: Hashing [Word64]
listHashing =
    Hashing
        { rootHash = \(Indirect k v) -> keyToListOfWord64 k <> v
        , combineHash = \(Indirect kl l) (Indirect kr r) ->
            keyToListOfWord64 kl <> l <> keyToListOfWord64 kr <> r
        }
verifyMHash :: Key -> Hash -> Pure Bool
verifyMHash = verifyM hashCodecs identityFromKV hashHashing

node :: Key -> a -> Indirect a
node jump value = Indirect{jump, value}

intHash :: Word64 -> Hash
intHash = mkHash . fromString . show

inserted
    :: (Foldable t, Ord k)
    => StandaloneCodecs k v a
    -> FromKV k v a
    -> Hashing a
    -> t (k, v)
    -> InMemoryDB
inserted codecs fromKV hashing =
    foldl'
        (\m (k, v) -> insert codecs fromKV hashing m k v)
        emptyInMemoryDB

allPaths :: Word64 -> [Key]
allPaths 0 = [[]]
allPaths c = do
    p <- allPaths (c - 1)
    [L : p, R : p]

genKey :: Gen Key
genKey = listOf $ elements [L, R]

genPaths :: Word64 -> Gen [Key]
genPaths n = shuffle (allPaths n)

genSomePaths :: Word64 -> Gen [Key]
genSomePaths n = fmap nub <$> listOf1 $ do
    let go 0 = return []
        go c = do
            d <- elements [L, R]
            ds <- go (c - 1)
            return (d : ds)
    go n

mkDeletionPath
    :: StandaloneCodecs k v a
    -> InMemoryDB
    -> Key
    -> Maybe (DeletionPath a)
mkDeletionPath codecs s k =
    fst
        . runPure s
        $ runTransactionUnguarded (pureDatabase codecs)
        $ newDeletionPath StandaloneCSMTCol k

data List e a
    = Cons e a
    deriving (Functor)

type ListOf e = Free (List e)

element :: e -> ListOf e ()
element x = liftF (Cons x ())

list :: ListOf a () -> [a]
list (Pure _) = []
list (Free (Cons x xs)) = x : list xs

evalPure :: InMemoryDB -> Pure b -> b
evalPure db p = fst $ runPure db p

-- pureBackendIdentity :: Backend (Pure Key v v) Key v v
-- pureBackendIdentity = pureBackend identityFromKV

evalPureFromEmptyDB :: Pure b -> b
evalPureFromEmptyDB = evalPure emptyInMemoryDB

runPureFromEmptyDB
    :: Pure b -> (b, InMemoryDB)
runPureFromEmptyDB = runPure emptyInMemoryDB

indirect :: Key -> a -> Indirect a
indirect = Indirect

insertIndirectM
    :: StandaloneCodecs Key a a
    -> Hashing a
    -> Indirect a
    -> Pure ()
insertIndirectM codecs hashing (Indirect k v) =
    insertM codecs identityFromKV hashing k v

insertWord64s :: [Indirect Word64] -> Pure ()
insertWord64s = mapM_ $ insertIndirectM word64Codecs word64Hashing

insertHashes :: [Indirect Hash] -> Pure ()
insertHashes = mapM_ $ insertIndirectM hashCodecs hashHashing

manyRandomPaths :: Gen [Key]
manyRandomPaths = scale (* 10) $ genSomePaths 256
