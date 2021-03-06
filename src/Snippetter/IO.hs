{-# LANGUAGE OverloadedStrings #-}

-- | Contains a bunch of functions related to file IO and interaction with the
-- outside world, including typeclasses that use it.
module Snippetter.IO 
  ( -- * Results, Errors, and Important Things
    FileError (..)
  , YamlError
  , FileResult
  , YamlResult
  , profileResult
  , profileWorldAction
  -- * World read/write classes
  , MonadReadWorld (..)
  , MonadWriteWorld (..)
  , NotifyType (..)
  , notifySuccess
  , notifyFailure
  , notifyProgress
  , notifyInfo
  -- * World read/write functions
  , yamlIfExists
  , yamlAsParams
  , paramsFromFile
  , mergeParams
  , pathWalk
  , pathWalkEndingIn
  , packRunProcess
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.Fixed
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import qualified Data.Yaml as Y
import Snippetter.Utilities
import System.Console.ANSI
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import System.IO.Error
import System.Process hiding (runProcess)
import System.FSNotify
import Twitch

-- | Possible errors when reading files.
data FileError
  = NotFound FilePath
  | IsInUse FilePath
  | AlreadyExists FilePath
  | NoReadPermission FilePath
  | NoWritePermission FilePath
  | ProcessFailure Int T.Text T.Text
  | EmptyProcess
  | OtherFileError FilePath IOError
  deriving (Eq)

instance Show FileError where
  show (NotFound f) = "\"" <> f <> "\" couldn't be found."
  show (IsInUse f) = "\"" <> f <> "\" is already in use."
  show (AlreadyExists f) =
    "\"" <> f <> "\" can't be modified as it already exists."
  show (NoReadPermission f) =
    "You don't have read permissions for \"" <> f <> "\"."
  show (NoWritePermission f) =
    "You don't have write permissions for \"" <> f <> "\"."
  show (ProcessFailure i stdout stderr) =
    "The process failed with exit code " <>
    show i <>
    ".\nStandard output:\n" <>
    T.unpack (indentFour stdout) <>
    "\nStandard error:\n" <> T.unpack (indentFour stderr)
  show EmptyProcess = "Attempted to execute a process consisting of nothing."
  show (OtherFileError f e) =
    "The following error occurred in \"" <>
    file <> "\":" <> show (ioeGetErrorType e)
    where
      file = unJust $ ioeGetFileName e <|> Just f

-- | Possible errors when decoding YAML from a file.
data YamlError
  = YamlFileError FileError
  | InvalidYamlFormat T.Text T.Text
  | OtherYamlError FilePath T.Text
  deriving (Eq)

instance Show YamlError where
  show (YamlFileError e) = show e
  show (InvalidYamlFormat f t) =
    "The following YAML doesn't represent \"" <>
    T.unpack f <> "\": \n" <> T.unpack (indentFour t)
  show (OtherYamlError f t) = "A YAML error occured: " <> T.unpack t

-- | The result a function that reads files.
type FileResult m a = Result FileError m a

-- | The result a function that reads and decodes YAML files.
type YamlResult m a = Result YamlError m a

-- | The @MonadReadWorld@ class is used to represent monads that can read files.
--   It can also be used for mocking purposes.
class Monad m =>
      MonadReadWorld m
  where
  getFileContents :: FilePath -> FileResult m T.Text
  fileExists :: FilePath -> m Bool
  dirExists :: FilePath -> m Bool
  directoryContents :: FilePath -> FileResult m [FilePath]
  fileModifyTime :: FilePath -> FileResult m UTCTime
  currentTime :: m UTCTime

-- | Represents different types of user notification.
data NotifyType
  = InProgress
  | Success
  | Failure
  | Info
  deriving (Show, Eq)

-- | The @MonadWriteWorld@ class is used to represent monads that interact with the
--   outside world, in ways that site builder generally should. This mainly
--   exists for mocking purposes.
class MonadReadWorld m =>
      MonadWriteWorld m
  where
  writeFile :: FilePath -> T.Text -> FileResult m ()
  deleteFile :: FilePath -> FileResult m ()
  copyFile :: FilePath -> FilePath -> FileResult m ()
  moveFile :: FilePath -> FilePath -> FileResult m ()
  runProcess :: T.Text -> [T.Text] -> T.Text -> m (ExitCode, T.Text, T.Text)
  notifyUser :: NotifyType -> T.Text -> m ()
  fileWatch :: FilePath -> m () -> m ()
  clearNotify :: m ()

-- | Pack @runProcess@ into a FileResult.
packRunProcess ::
     MonadWriteWorld m => [T.Text] -> T.Text -> FileResult m ()
packRunProcess [] stdin = resultE EmptyProcess
packRunProcess process stdin = do
  let p = head process
  let args = tail process
  (x, y, z) <- resultLift $ runProcess p args stdin
  case x of
    ExitSuccess -> return ()
    ExitFailure ii -> resultE $ ProcessFailure ii y z

-- | Helper function for IO operations
tryIO :: IO a -> (a -> b) -> (IOException -> c) -> Result c IO b
tryIO toDo sucWrap errWrap = do
  result <- liftIO $ try toDo
  case result of
    Right r -> return $ sucWrap r
    Left l -> resultE $ errWrap l

instance MonadReadWorld IO where
  getFileContents path = tryIO (TIO.readFile path) id (rewrapReadError path)
  fileExists = doesFileExist
  dirExists = doesDirectoryExist
  fileModifyTime path =
    tryIO (getModificationTime path) id (rewrapReadError path)
  directoryContents path =
    tryIO (getDirectoryContents path) id (rewrapReadError path)
  currentTime = getCurrentTime

instance MonadWriteWorld IO where
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
  fileWatch dir action = do
    -- this mvar exists to prevent multiple threads building the site
    -- simultaneously
    lock <- newMVar ()
    withManager $ \mgr -> do
      watchTree mgr dir (const True) $ \file -> do
        isLocked <- not <$> isEmptyMVar lock
        when (isLocked) $ do
          _ <- takeMVar lock
          case file of
            (Added path _ _) -> do
              isFile <- fileExists path
              when isFile action
            (Modified path _ _) -> do
              isFile <- fileExists path
              when isFile action
            otherwise -> return ()
          putMVar lock ()
      forever $ threadDelay 1000000
  clearNotify = return ()

-- | Shorthand for @notifyUser InProgress@.
notifyProgress :: MonadWriteWorld m => T.Text -> m ()
notifyProgress = notifyUser InProgress

-- | Shorthand for @notifyUser Success@.
notifySuccess :: MonadWriteWorld m => T.Text -> m ()
notifySuccess = notifyUser Success

-- | Shorthand for @notifyUser Failure@.
notifyFailure :: MonadWriteWorld m => T.Text -> m ()
notifyFailure = notifyUser Failure

-- | Shorthand for @notifyUser Failure@.
notifyInfo :: MonadWriteWorld m => T.Text -> m ()
notifyInfo = notifyUser Info

-- | Notifies the user by printing to the console.
notifyUserIO :: NotifyType -> T.Text -> IO ()
notifyUserIO nt text = do
  let lookup' a l = fromJust $ lookup a l
  let colour = lookup' nt [(Success, Green), (Failure, Red)]
  case nt of
    InProgress -> TIO.putStr text
    Info -> TIO.putStr text
    _ -> do
      setSGR
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid colour]
      TIO.putStrLn text
      setSGR
        [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid White]
      hFlush stdout

-- | Wrap an IOException in a FileError. Used when reading files.
rewrapReadError :: FilePath -> IOException -> FileError
rewrapReadError defaultPath e
  | isPermissionError e = NoReadPermission path
  | isDoesNotExistError e = NotFound path
  | isAlreadyInUseError e = IsInUse path
  | otherwise = OtherFileError path e
  where
    path = unJust $ ioeGetFileName e <|> Just defaultPath

-- | Wrap an IOException in a FileError. Used when modifying files.
rewrapWriteError :: FilePath -> IOException -> FileError
rewrapWriteError defaultPath e
  | isPermissionError e = NoWritePermission path
  | isDoesNotExistError e = NotFound path
  | isAlreadyExistsError e = AlreadyExists path
  | isAlreadyInUseError e = IsInUse path
  | otherwise = OtherFileError path e
  where
    path = unJust $ ioeGetFileName e <|> Just defaultPath

-- | Execute a @Result@, printing the time it took to execute, along with
-- whether it succeeded or failed (and the error if the latter occured).
profileResult ::
     (MonadWriteWorld m, Show e) => T.Text -> Result e m a -> Result e m a
profileResult desc act = do
  beforeTime <- resultLift currentTime
  resultLift $ notifyProgress desc
  result <- resultLift $ runResult act
  afterTime <- resultLift currentTime
  let timeCode = formatTime $ diffUTCTime afterTime beforeTime
  case result of
    Right r -> resultLift (notifySuccess $ "OK, " <> timeCode) >> return r
    Left l ->
      resultLift
        (notifyFailure $
         "FAIL (" <> timeCode <> ")\n" <> indentFour (T.pack $ show l)) >>
      resultE l

-- | Execute some action that reads/writes to the world, printing the time it
-- took to execute.
profileWorldAction :: MonadWriteWorld m => m a -> m ()
profileWorldAction act = do
  beforeTime <- currentTime
  act
  afterTime <- currentTime
  let timeCode = formatTime $ diffUTCTime afterTime beforeTime
  notifySuccess $ "Done, " <> timeCode <> " total."

-- | Format a time difference to a human readable format.
formatTime :: NominalDiffTime -> T.Text
formatTime time =
  if final == ""
    then "0ms"
    else final
  where
    dTime = (realToFrac time :: Double) * 1000
    milliT = (dTime `div'` 1) `mod'` 1000
    secT = (dTime `div'` 1000) `mod'` 60
    minT = dTime `div'` 60000
    tf num suffix =
      if num > 0
        then T.pack (show num) <> suffix
        else ""
    milli = tf milliT "ms"
    sec = tf secT "s "
    min = tf minT "m "
    final = min <> sec <> milli

-- | Decodes an Aeson-parsable ADT from the supplied text.
decodeYaml :: Y.FromJSON a => T.Text -> Either Y.ParseException a
decodeYaml str = Y.decodeEither' $ B.pack $ T.unpack str

-- | Retrieves contents of the specified file, and maps possible errors to a
--   new type.
getFileContents' ::
     MonadReadWorld m => FilePath -> (FileError -> e') -> Result e' m T.Text
getFileContents' path func = getFileContents path `mapResultError` func

-- | Loads an Aeson-parsable ADT from the supplied YAML file.
yamlIfExists ::
     (MonadReadWorld m, Y.FromJSON a) => T.Text -> FilePath -> YamlResult m a
yamlIfExists datatype path = do
  contents <- getFileContents' path $ \x -> YamlFileError x
  case decodeYaml contents of
    Left l -> resultE $ InvalidYamlFormat datatype contents
    Right r -> return r

-- | Get all files in a directory recursively.
-- Adapted from http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
pathWalk :: MonadReadWorld m => FilePath -> FileResult m [FilePath]
pathWalk folder = do
  names <- directoryContents folder
  let properNames = filter (`notElem` [".", ".."]) names
  paths <-
    forM properNames $ \name -> do
      let path = folder </> name
      isDirectory <- resultLift $ dirExists path
      if isDirectory
        then pathWalk path
        else return [path]
  return (concat paths)

-- | Get all files in a directory recursively if they end in the given suffix.
pathWalkEndingIn ::
     MonadReadWorld m => FilePath -> FilePath -> FileResult m [FilePath]
pathWalkEndingIn folder ending = do
  walked <- pathWalk folder
  let f = isSuffixOf ending
  return $ filter f walked

-- | Load a YAML file as a list of @Params@.
yamlAsParams :: MonadReadWorld m => FilePath -> YamlResult m [Params]
yamlAsParams path =
   yamlIfExists "List of parameters" path
       :: MonadReadWorld m => YamlResult m [Params]

-- | Load all paramaters from a (possible) paramater file as @PathedParams@.
paramsFromFile :: MonadReadWorld m => FilePath -> YamlResult m [PathedParams]
paramsFromFile file = do
  values <- yamlAsParams file
  let addPath x = PathedParams x $ Just file
  return $ map addPath values

-- | Load all parameters from files, merge them with the given list, and add
-- missing attributes that exist in another set of parameters, associating them
-- all with relevant filenames.
mergeParams ::
     MonadReadWorld m =>
     [PathedParams] ->
     [FilePath] ->
     Params ->
     YamlResult m [PathedParams]
mergeParams params files def = do
    paramsFromFile <- concat <$> mapM paramsFromFile files
    let allParams = params ++ paramsFromFile
    return $ map (pathedParamDefault def) allParams
