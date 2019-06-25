module Snippetter where

import Control.Monad.Trans.Except
import Snippetter.LayoutBase
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.HashMap.Strict

executeSiteAction :: SiteAction -> IO ()
executeSiteAction a@(Build m mp f) = do
    evaluated <- runExceptT $ evaluateBuildAction a
    case evaluated of
      Left l  -> putStrLn $ show l
      Right r -> TIO.writeFile f r >> (putStrLn $ "Successfully created " ++ f)
executeSiteAction _                = undefined

executeLayoutFile :: FilePath -> HashMap T.Text Macro -> IO ()
executeLayoutFile path map = do
    executed <- runExceptT $ loadSiteActions path map
    case executed of
      Left l  -> putStrLn $ show l
      Right r -> mapM_ executeSiteAction r
