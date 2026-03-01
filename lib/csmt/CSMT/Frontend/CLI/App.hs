-- |
-- Module      : CSMT.Frontend.CLI.App
-- Description : Interactive CLI for CSMT operations
-- Copyright   : (c) Paolo Veronelli, 2026
-- License     : Apache-2.0
--
-- Command-line interface for interacting with a CSMT database.
-- Supports inserting, deleting, and querying key-value pairs,
-- as well as generating and verifying inclusion proofs.
module CSMT.Frontend.CLI.App
    ( main
    ) where

import CSMT.Backend.RocksDB
    ( RunRocksDB (RunRocksDB)
    , standaloneRocksDBDatabase
    , withRocksDB
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCodecs (..)
    )
import CSMT.Hashes
    ( Hash
    , byteStringToKey
    , delete
    , fromKVHashes
    , generateInclusionProof
    , insert
    , isoHash
    , renderHash
    , root
    , verifyInclusionProof
    )
import CSMT.Interface
    ( Indirect (..)
    , Key
    )
import CSMT.Interface qualified as I
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteArray.Encoding
    ( Base (Base64)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Database.KV.Transaction
    ( RunTransaction (..)
    , newRunTransaction
    , query
    )
import Database.RocksDB (BatchOp, ColumnFamily)
import OptEnvConf
    ( Parser
    , argument
    , auto
    , env
    , help
    , long
    , metavar
    , option
    , reader
    , runParser
    , setting
    , str
    )
import OptEnvConf.Setting qualified as Opt
import Paths_mts (version)
import System.Console.Haskeline
    ( defaultSettings
    , getInputLine
    , runInputT
    )
import System.IO
    ( BufferMode (LineBuffering)
    , hIsTerminalDevice
    , hSetBuffering
    , isEOF
    , stdin
    , stdout
    )

data Command
    = -- | Insert key value
      I ByteString ByteString
    | -- | Delete key
      D ByteString
    | -- | Query inclusion proof for key
      Q ByteString
    | -- | Query hash at partial key
      QB (Maybe Key)
    | -- | Verify inclusion proof (self-contained)
      V ByteString
    | -- | Query key-value pair
      W ByteString
    | -- | Query root hash
      R
    | -- | Comment
      C
    | -- | Show key directions
      K ByteString

data Error
    = EmptyProof
    | TreeEmpty
    | InvalidProofFormat
    | InvalidKeyFormat
    | NoProofFound
    | NoNodeFound
    | KeyNotFound
    | DeletedKey
    | AddedKey
    | Valid
    | Invalid
    | UnknownCommand
    | Comment
    deriving (Show)

renderError :: Error -> String
renderError EmptyProof = "Empty proof for the first insertion"
renderError TreeEmpty = "Tree is empty"
renderError InvalidProofFormat = "Invalid proof format"
renderError InvalidKeyFormat = "Invalid key format"
renderError NoProofFound = "No proof found"
renderError NoNodeFound = "No node found at the given key"
renderError KeyNotFound = "Key not found"
renderError DeletedKey = "Deleted key, exclusion proof generation not implemented"
renderError AddedKey = "Added key, inclusion proof generated"
renderError Valid = "Valid proof"
renderError Invalid = "Invalid proof"
renderError UnknownCommand = helpInteractive
renderError Comment = ""

parseCommand :: ByteString -> Maybe Command
parseCommand line =
    case BC.words line of
        ["i", k, v] -> Just (I k v)
        ["d", k] -> Just (D k)
        ["q", k] -> Just (Q k)
        ["p"] -> Just (QB $ Just [])
        ["p", ks] -> Just (QB $ parseLRKey ks)
        ["w", k] -> Just (W k)
        ["v", proof] -> Just (V proof)
        ["r"] -> Just R
        ["k", key] -> Just (K key)
        "#" : _comment -> Just C
        _ -> Nothing

parseLRKey :: ByteString -> Maybe Key
parseLRKey = traverse step . BC.unpack
  where
    step 'L' = Just I.L
    step 'R' = Just I.R
    step _ = Nothing

type Prompt = Maybe ByteString

mkPrompt :: Bool -> String -> Prompt
mkPrompt isPiped cmd = if isPiped then Nothing else Just (BC.pack cmd)

printHash :: Prompt -> ByteString -> IO ()
printHash (Just prompt) what =
    BC.putStrLn . ((prompt <> ": ") <>) . convertToBase Base64 $ what
printHash Nothing what = BC.putStrLn . convertToBase Base64 $ what

readHash :: ByteString -> Maybe ByteString
readHash bs = case convertFromBase Base64 bs of
    Left _ -> Nothing
    Right h -> Just h

data Output
    = Binary String ByteString
    | Text ByteString
    | Node (Indirect Hash)
    | ErrorMsg Error

core
    :: Bool
    -> RunTransaction
        IO
        ColumnFamily
        (Standalone ByteString ByteString Hash)
        BatchOp
    -> String
    -> IO ()
core isPiped (RunTransaction run) l' = do
    r <- case parseCommand $ BC.pack l' of
        Just (I k v) -> do
            run $ insert fromKVHashes StandaloneKVCol StandaloneCSMTCol k v
            pure $ ErrorMsg AddedKey
        Just (D k) -> do
            run $ delete fromKVHashes StandaloneKVCol StandaloneCSMTCol k
            pure $ ErrorMsg DeletedKey
        Just (Q k) -> do
            r <-
                run
                    $ generateInclusionProof
                        fromKVHashes
                        StandaloneKVCol
                        StandaloneCSMTCol
                        k
            pure $ case r of
                Just (_v, proof) -> Binary "proof" proof
                Nothing -> ErrorMsg KeyNotFound
        Just (QB mk) -> do
            case mk of
                Nothing -> pure $ ErrorMsg InvalidKeyFormat
                Just k -> do
                    mv <- run $ query StandaloneCSMTCol k
                    pure $ case mv of
                        Just v -> Node v
                        Nothing -> ErrorMsg NoNodeFound
        Just R -> do
            r <- run $ root StandaloneCSMTCol
            pure $ case r of
                Just rootHash -> Binary "root" rootHash
                Nothing -> ErrorMsg TreeEmpty
        Just (V proof) -> do
            case readHash proof of
                Just decoded ->
                    pure
                        $ ErrorMsg
                        $ if verifyInclusionProof decoded then Valid else Invalid
                Nothing -> pure $ ErrorMsg InvalidProofFormat
        Just (W k) -> do
            mv <- run $ query StandaloneKVCol k
            pure $ case mv of
                Just v -> Text v
                Nothing -> ErrorMsg KeyNotFound
        Just C -> pure $ ErrorMsg Comment
        Just (K k) -> pure $ Text $ BC.pack $ byteStringToKey k >>= show
        Nothing -> pure $ ErrorMsg UnknownCommand
    case r of
        Binary prompt hash -> reportBinary prompt hash
        Text txt -> BC.putStrLn txt
        ErrorMsg e -> reportError' e
        Node node -> do
            BC.putStrLn $ renderKey $ jump node
            reportBinary "value" (renderHash $ value node)
  where
    reportBinary prompt = printHash (mkPrompt isPiped prompt)
    reportError' e
        | isPiped = print e
        | otherwise = putStrLn $ renderError e

renderKey :: Key -> ByteString
renderKey = BC.pack . fmap dirToByte
  where
    dirToByte I.L = 'L'
    dirToByte I.R = 'R'
data Options = Options
    { optDbPath :: FilePath
    , optCSMTMaxFiles :: Int
    , optKVMaxFiles :: Int
    }

parseDbPath :: Parser FilePath
parseDbPath =
    setting
        [ argument
        , metavar "DIR"
        , help "Path to RocksDB database"
        , reader str
        , env "CSMT_DB_PATH"
        ]

parseCSMTMaxFiles :: Parser Int
parseCSMTMaxFiles =
    setting
        [ option
        , metavar "INT"
        , long "csmt-max-files"
        , help "Maximum number of CSMT files"
        , reader auto
        , env "CSMT_MAX_FILES"
        , Opt.value 1
        ]

parseKVMaxFiles :: Parser Int
parseKVMaxFiles =
    setting
        [ option
        , metavar "INT"
        , long "kv-max-files"
        , help "Maximum number of KV files"
        , reader auto
        , env "KV_MAX_FILES"
        , Opt.value 1
        ]

optionsParser :: Parser Options
optionsParser =
    Options
        <$> parseDbPath
        <*> parseCSMTMaxFiles
        <*> parseKVMaxFiles

codecs :: StandaloneCodecs ByteString ByteString Hash
codecs =
    StandaloneCodecs
        { keyCodec = id
        , valueCodec = id
        , nodeCodec = isoHash
        }

-- | Entry point for the CSMT CLI application.
main :: IO ()
main = do
    Options{optDbPath, optCSMTMaxFiles, optKVMaxFiles} <-
        runParser version "csmt" optionsParser
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    withRocksDB optDbPath optCSMTMaxFiles optKVMaxFiles $ \(RunRocksDB run) -> do
        runT <- do
            db <- run $ standaloneRocksDBDatabase codecs
            newRunTransaction db
        isPiped <- checkPipeline
        if isPiped
            then fix $ \loop -> do
                eof <- isEOF
                unless eof $ do
                    line <- BC.getLine
                    core isPiped runT (BC.unpack line)
                    loop
            else do
                putStrLn helpInteractive
                runInputT defaultSettings $ fix $ \loop -> do
                    mlline <- getInputLine "\n> "
                    case mlline of
                        Nothing -> return ()
                        Just line -> do
                            lift $ core isPiped runT line
                            loop

checkPipeline :: IO Bool
checkPipeline = not <$> hIsTerminalDevice stdin

helpInteractive :: String
helpInteractive =
    unlines
        [ "Commands:"
        , "  i <key> <value> Insert key-value pair"
        , "  q <key>         Generate inclusion proof for key"
        , "  v <proof>       Verify inclusion proof"
        , "  w <key>         Query value for key"
        , "  d <key>         Delete key"
        , "  p <key>         Query node at partial key (LRLRLL...)"
        , "  r               Print root hash"
        , "  k <key>         Show key as directions"
        , "  # <comment>     Comment line"
        ]
