import CSMT.Backend.RocksDB
    ( RunRocksDB (..)
    , standaloneRocksDBDatabase
    , withRocksDB
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCodecs (..)
    )
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , insert
    , isoHash
    , mkHash
    , renderHash
    )
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Database.KV.Transaction qualified as Transaction

codecs :: StandaloneCodecs ByteString ByteString Hash
codecs =
    StandaloneCodecs
        { keyCodec = id
        , valueCodec = id
        , nodeCodec = isoHash
        }

main :: IO ()
main =
    withRocksDB "tmp/sl" 1 1 $ \(RunRocksDB run) ->
        forM_ [1 :: Int ..] $ \i -> do
            when (i `mod` 1000 == 0)
                $ putStrLn
                $ "Inserted " ++ show i ++ " items."
            database <- run $ standaloneRocksDBDatabase codecs
            run
                $ Transaction.runTransactionUnguarded database
                $ insert
                    fromKVHashes
                    StandaloneKVCol
                    StandaloneCSMTCol
                    (duplicate 2 $ renderHash . mkHash $ B.pack $ show i)
                    (duplicate 10 $ renderHash . mkHash $ B.pack $ show i)

duplicate :: Int -> ByteString -> ByteString
duplicate n bs = B.concat (replicate n bs)
