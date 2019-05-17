module Hssb.Utilities where

import Hssb.Layout.Types
import Data.Aeson.Types (Object, Value (Object, String))
import Data.Hashable
import Data.HashMap.Strict (HashMap, lookup)
import Data.List.Split
import Prelude hiding (lookup)
import System.FilePath
import qualified Data.Text as T

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither fail (Nothing) = Left fail
maybeToEither _    (Just a)  = Right a

lookupEither :: (Eq k, Hashable k) => e -> k -> HashMap k v -> Either e v
lookupEither def key value =
    case (lookup key value) of
      Nothing -> Left def
      Just a  -> Right a

lookupParams :: (Value -> Maybe a) -> String -> MacroParams -> Either DocError a
lookupParams get key map = do
    thing <- lookupEither (AbsentKey key) keyT map
    maybeToEither (WrongKeyType key) (get thing)
    where keyT = T.pack key

unString :: Value -> Maybe String
unString (String s) = Just $ T.unpack s
unString _          = Nothing

unObject :: Value -> Maybe (HashMap T.Text Value)
unObject (Object o) = Just o
unObject _          = Nothing

lookupString :: String -> MacroParams -> Either DocError String
lookupString = lookupParams unString

lookupObject :: String -> MacroParams -> Either DocError (HashMap T.Text Value)
lookupObject = lookupParams unObject

addString :: String -> Action
addString s =  Action $ Add $ T.pack s

add :: Contentable c => c -> Action
add c = Action $ Add c

replace :: Actionable a => String -> a -> Action
replace t d = Action $ Replace (T.pack t) d

replaceText :: String -> String -> Action
replaceText t d = replace t $ asDoc $ T.pack d

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
