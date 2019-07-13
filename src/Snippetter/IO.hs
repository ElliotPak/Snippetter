{-# LANGUAGE StandaloneDeriving #-}

module Snippetter.IO where

import Snippetter.Utilities
import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import System.Directory (doesFileExist)
import System.IO.Error
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as B

data FileError =
    NotFound FilePath
  | IsInUse FilePath
  | NoReadPermission FilePath
  | NoWritePermission FilePath
  | OtherFileError FilePath IOError
    deriving (Show, Eq)

data YamlError =
    YamlFileError FileError
  | InvalidYamlFormat FilePath
  | OtherYamlError FilePath T.Text
    deriving (Show, Eq)

type FileResult m a = ExceptT FileError m a
type YamlResult m a = ExceptT YamlError m a

-- | The @MonadReadFile@ class is used to represent monads that can read files.
--   It can also be used for mocking purposes.
class Monad m => MonadReadFile m where
    getFileContents :: FilePath -> ExceptT FileError m T.Text
    fileExists :: FilePath -> m Bool

rewrapReadError :: FilePath -> IOException -> FileError
rewrapReadError path e
  | isPermissionError e = NoReadPermission path
  | otherwise           = OtherFileError path e

instance MonadReadFile IO where
    getFileContents path = do
        result <- liftIO $ try (Prelude.readFile path)
        case result of
          Right r -> return $ T.pack r
          Left l  -> throwE $ rewrapReadError path l
    fileExists = doesFileExist

-- | Decodes an Aeson-parsable ADT from the supplied text.
decodeYaml :: Y.FromJSON a => T.Text -> Either Y.ParseException a
decodeYaml str = Y.decodeEither' $ B.pack $ T.unpack str

mapResultError :: Monad m => ExceptT e m a -> (e -> e') -> ExceptT e' m a
mapResultError except mapping = ExceptT answer
    where ran = runExceptT except
          answer = liftM (mapLeft mapping) ran

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
