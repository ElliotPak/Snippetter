module Hssb.Utilities where

import Hssb.Layout.Types
import Data.Aeson.Types (Object, Value (Object, String))
import Data.HashMap.Strict (HashMap, lookup)
import Data.List.Split
import Prelude hiding (lookup)
import System.FilePath
import qualified Data.Text as T

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither fail (Nothing) = Left fail
maybeToEither _    (Just a)  = Right a

lookupEither :: (Value -> Maybe a) -> String -> MacroParams -> Either DocError a
lookupEither get key map = do
    thing <- maybeToEither (AbsentKey key) (lookup keyT map)
    maybeToEither (WrongKeyType key) (get thing)
    where keyT = T.pack key

unString :: Value -> Maybe String
unString (String s) = Just $ T.unpack s
unString _          = Nothing

unObject :: Value -> Maybe (HashMap T.Text Value)
unObject (Object o) = Just o
unObject _          = Nothing

lookupString :: String -> MacroParams -> Either DocError String
lookupString = lookupEither unString

lookupObject :: String -> MacroParams -> Either DocError (HashMap T.Text Value)
lookupObject = lookupEither unObject

add :: Contentable c => c -> Action
add c = Action $ Add $ Content $ c

replace :: Actionable a => String -> a -> Action
replace t d = Action (Replace (T.pack t) (Action d))

relativePath :: FilePath -> FilePath -> FilePath
relativePath from to = rel "" splitFrom splitTo
    where
        splitTo   = splitOn [pathSeparator] to
        splitFrom = splitOn [pathSeparator] from
        rel path []     []     = path
        rel path (x:[]) []     = path
        rel path (x:xs) []     = rel (".." </> path) xs []
        rel path []     (y:ys) = rel (path </> y) [] ys
        rel path (x:xs) (y:ys)
          | path /= "" && xs /= [] = rel (".." </> path </> y) xs ys
          | path /= "" && xs == [] = rel (path </> y) xs ys
          | x == y                 = rel path xs ys
          | otherwise              = rel (".." </> path </> y) xs ys
