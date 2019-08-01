{-# LANGUAGE StandaloneDeriving #-}

module Snippetter.IO where

import Snippetter.Utilities
import Control.Exception
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import System.Directory
import System.Process
import System.Exit
import System.IO.Error
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as B

-- | Possible errors when reading files.
data FileError =
    NotFound FilePath
  | IsInUse FilePath
  | AlreadyExists FilePath
  | NoReadPermission FilePath
  | NoWritePermission FilePath
  | OtherFileError FilePath IOError
    deriving (Show, Eq)

-- | Possible errors when decoding YAML from a file.
data YamlError =
    YamlFileError FileError
  | InvalidYamlFormat FilePath
  | OtherYamlError FilePath T.Text
    deriving (Show, Eq)

-- | The result a function that reads files.
type FileResult m a = ExceptT FileError m a

-- | The result a function that reads and decodes YAML files.
type YamlResult m a = ExceptT YamlError m a

-- | The @MonadReadFile@ class is used to represent monads that can read files.
--   It can also be used for mocking purposes.
class Monad m => MonadReadFile m where
    -- | Retrieves contents of the specified file. Any errors are represented
    --   by a FileResult.
    getFileContents :: FilePath -> ExceptT FileError m T.Text
    -- | Determines if the specified file exists.
    fileExists :: FilePath -> m Bool

-- | The @MonadWorld@ class is used to represent monads that interact with the
--   outside world, in ways that site builder generally should. This mainly
--   exists for mocking purposes.
class MonadReadFile m => MonadWorld m where
    -- | Write text to a file.
    writeFile  :: FilePath -> T.Text -> ExceptT FileError m ()
    -- | Delete the given file.
    deleteFile :: FilePath -> ExceptT FileError m ()
    -- | Copy the first file to the location of the second.
    copyFile   :: FilePath -> FilePath -> ExceptT FileError m ()
    -- | Move the first file to the location of the second.
    moveFile   :: FilePath -> FilePath -> ExceptT FileError m ()
    -- | Run the specified process, with the given arguments and stdin input.
    runProcess :: FilePath -> [T.Text] -> T.Text -> m (ExitCode, T.Text, T.Text)

-- | Helper function for IO operations
tryIO :: IO a -> (a -> b) -> (IOException -> c) -> ExceptT c IO b
tryIO toDo sucWrap errWrap = do
    result <- liftIO $ try toDo
    case result of 
      Right r -> return $ sucWrap r
      Left l  -> throwE $ errWrap l

instance MonadReadFile IO where
    getFileContents path = do
        tryIO (TIO.readFile path) id (rewrapReadError path)
    fileExists = doesFileExist

instance MonadWorld IO where
    writeFile path contents =
        tryIO (TIO.writeFile path contents) (const ()) (rewrapWriteError path)
    deleteFile path =
        tryIO (removeFile path) (const ()) (rewrapWriteError path)
    copyFile from to =
        tryIO (System.Directory.copyFile from to) (const ()) (rewrapWriteError from)
    moveFile from to =
        tryIO (renameFile from to) (const ()) (rewrapWriteError from)
    runProcess process args stdin = do
        let textify (a, b, c) = (a, T.pack b, T.pack c)
        let args' = map T.unpack args
        let stdin'= T.unpack stdin
        results <- readProcessWithExitCode process args' stdin'
        return $ textify results

-- | Wrap an IOException in a FileError. Used when reading files.
rewrapReadError :: FilePath -> IOException -> FileError
rewrapReadError defaultPath e
  | isPermissionError e = NoReadPermission path
  | isDoesNotExistError e = NotFound path
  | isAlreadyInUseError e = IsInUse path
  | otherwise           = OtherFileError path e
    where unRight (Just x) = x
          path = unRight $ ioeGetFileName e <|> Just defaultPath

-- | Wrap an IOException in a FileError. Used when modifying files.
rewrapWriteError :: FilePath -> IOException -> FileError
rewrapWriteError defaultPath e
  | isPermissionError e = NoWritePermission path
  | isDoesNotExistError e = NotFound path
  | isAlreadyExistsError e = AlreadyExists path
  | isAlreadyInUseError e = IsInUse path
  | otherwise           = OtherFileError path e
    where unRight (Just x) = x
          path = unRight $ ioeGetFileName e <|> Just defaultPath

-- | Decodes an Aeson-parsable ADT from the supplied text.
decodeYaml :: Y.FromJSON a => T.Text -> Either Y.ParseException a
decodeYaml str = Y.decodeEither' $ B.pack $ T.unpack str

-- | Retrieves contents of the specified file, and maps possible errors to a
--   new type.
getFileContents' :: MonadReadFile m => FilePath -> (FileError -> e') -> ExceptT e' m T.Text
getFileContents' path func =
    (getFileContents path) `mapResultError` func

-- | Loads an Aeson-parsable ADT from the supplied YAML file.
yamlIfExists :: (MonadReadFile m, Y.FromJSON a) =>
    FilePath -> YamlResult m a
yamlIfExists path = do
    contents <- getFileContents' path $ \x -> YamlFileError x
    case (decodeYaml contents) of
      Left l  -> throwE $ InvalidYamlFormat path
      Right r -> return r
