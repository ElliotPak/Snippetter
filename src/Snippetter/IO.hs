{-# LANGUAGE StandaloneDeriving #-}

module Snippetter.IO where

import Control.Applicative
import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import Snippetter.Utilities
import System.Console.ANSI
import System.Directory
import System.Exit
import System.IO.Error
import System.Process hiding (runProcess)

-- | Possible errors when reading files.
data FileError
  = NotFound FilePath
  | IsInUse FilePath
  | AlreadyExists FilePath
  | NoReadPermission FilePath
  | NoWritePermission FilePath
  | ProcessFailure Int T.Text T.Text
  | OtherFileError FilePath IOError
  deriving (Show, Eq)

-- | Possible errors when decoding YAML from a file.
data YamlError
  = YamlFileError FileError
  | InvalidYamlFormat FilePath
  | OtherYamlError FilePath T.Text
  deriving (Show, Eq)

-- | The result a function that reads files.
type FileResult m a = ExceptT FileError m a

-- | The result a function that reads and decodes YAML files.
type YamlResult m a = ExceptT YamlError m a

-- | The @MonadReadFile@ class is used to represent monads that can read files.
--   It can also be used for mocking purposes.
class Monad m =>
      MonadReadFile m
    -- | Retrieves contents of the specified file. Any errors are represented
    --   by a FileResult.
  where
  getFileContents :: FilePath -> ExceptT FileError m T.Text
    -- | Determines if the specified file exists.
  fileExists :: FilePath -> m Bool

-- | Represents different types of user notification.
data NotifyType
  = InProgress
  | Success
  | Failure
  deriving (Show, Eq)

-- | The @MonadWorld@ class is used to represent monads that interact with the
--   outside world, in ways that site builder generally should. This mainly
--   exists for mocking purposes.
class MonadReadFile m =>
      MonadWorld m
    -- | Write text to a file.
  where
  writeFile :: FilePath -> T.Text -> ExceptT FileError m ()
    -- | Delete the given file.
  deleteFile :: FilePath -> ExceptT FileError m ()
    -- | Copy the first file to the location of the second.
  copyFile :: FilePath -> FilePath -> ExceptT FileError m ()
    -- | Move the first file to the location of the second.
  moveFile :: FilePath -> FilePath -> ExceptT FileError m ()
    -- | Run the specified process, with the given arguments and stdin input.
  runProcess :: T.Text -> [T.Text] -> T.Text -> m (ExitCode, T.Text, T.Text)
    -- | Notify the user of something.
  notifyUser :: NotifyType -> T.Text -> m ()
    -- | Clear the last notification sent.
  clearNotify :: m ()

-- | Pack @runProcess@ into a FileResult.
packRunProcess ::
     MonadWorld m => T.Text -> [T.Text] -> T.Text -> ExceptT FileError m ()
packRunProcess process args stdin = do
  (x, y, z) <- lift $ runProcess process args stdin
  case x of
    ExitSuccess -> return ()
    ExitFailure ii -> throwE $ ProcessFailure ii y z

-- | Helper function for IO operations
tryIO :: IO a -> (a -> b) -> (IOException -> c) -> ExceptT c IO b
tryIO toDo sucWrap errWrap = do
  result <- liftIO $ try toDo
  case result of
    Right r -> return $ sucWrap r
    Left l -> throwE $ errWrap l

instance MonadReadFile IO where
  getFileContents path = tryIO (TIO.readFile path) id (rewrapReadError path)
  fileExists = doesFileExist

instance MonadWorld IO where
  writeFile path contents =
    tryIO (TIO.writeFile path contents) id (rewrapWriteError path)
  deleteFile path = tryIO (removeFile path) id (rewrapWriteError path)
  copyFile from to =
    tryIO (System.Directory.copyFile from to) id (rewrapWriteError from)
  moveFile from to = tryIO (renameFile from to) id (rewrapWriteError from)
  runProcess process args stdin = do
    let textify (a, b, c) = (a, T.pack b, T.pack c)
    let process' = T.unpack process
    let args' = map T.unpack args
    let stdin' = T.unpack stdin
    results <- readProcessWithExitCode process' args' stdin'
    return $ textify results
  notifyUser = notifyUserIO
  clearNotify = return ()

-- | Shorthand for @notifyUser InProgress@.
notifyProgress :: MonadWorld m => T.Text -> m ()
notifyProgress = notifyUser InProgress

-- | Shorthand for @notifyUser Success@.
notifySuccess :: MonadWorld m => T.Text -> m ()
notifySuccess = notifyUser Success

-- | Shorthand for @notifyUser Failure@.
notifyFailure :: MonadWorld m => T.Text -> m ()
notifyFailure = notifyUser Failure

notifyUserIO :: NotifyType -> T.Text -> IO ()
notifyUserIO nt text = do
  let lookup' a l = fromJust $ lookup a l
  let colour =
        lookup' nt [(InProgress, Yellow), (Success, Green), (Failure, Red)]
  let marker =
        lookup' nt [(InProgress, "..."), (Success, "Ok!"), (Failure, "!!!")]
  setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid colour]
  putStr $ "[" <> marker <> "] "
  setSGR [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
  TIO.putStrLn text

-- | Wrap an IOException in a FileError. Used when reading files.
rewrapReadError :: FilePath -> IOException -> FileError
rewrapReadError defaultPath e
  | isPermissionError e = NoReadPermission path
  | isDoesNotExistError e = NotFound path
  | isAlreadyInUseError e = IsInUse path
  | otherwise = OtherFileError path e
  where
    unRight (Just x) = x
    path = unRight $ ioeGetFileName e <|> Just defaultPath

-- | Wrap an IOException in a FileError. Used when modifying files.
rewrapWriteError :: FilePath -> IOException -> FileError
rewrapWriteError defaultPath e
  | isPermissionError e = NoWritePermission path
  | isDoesNotExistError e = NotFound path
  | isAlreadyExistsError e = AlreadyExists path
  | isAlreadyInUseError e = IsInUse path
  | otherwise = OtherFileError path e
  where
    unRight (Just x) = x
    path = unRight $ ioeGetFileName e <|> Just defaultPath

-- | Decodes an Aeson-parsable ADT from the supplied text.
decodeYaml :: Y.FromJSON a => T.Text -> Either Y.ParseException a
decodeYaml str = Y.decodeEither' $ B.pack $ T.unpack str

-- | Retrieves contents of the specified file, and maps possible errors to a
--   new type.
getFileContents' ::
     MonadReadFile m => FilePath -> (FileError -> e') -> ExceptT e' m T.Text
getFileContents' path func = getFileContents path `mapResultError` func

-- | Loads an Aeson-parsable ADT from the supplied YAML file.
yamlIfExists :: (MonadReadFile m, Y.FromJSON a) => FilePath -> YamlResult m a
yamlIfExists path = do
  contents <- getFileContents' path $ \x -> YamlFileError x
  case decodeYaml contents of
    Left l -> throwE $ InvalidYamlFormat path
    Right r -> return r
