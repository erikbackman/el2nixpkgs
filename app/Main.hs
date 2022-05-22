{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Lisp
import System.Directory
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Unable to parse lisp"
    (style : config : _) -> do
      readable <- doesFileExist config
      when readable $ do
        src <- readFile' config
        print $ fromMaybe "Unsupported" (extractWith style src)

newtype Package = Package {fromPackage :: String}
  deriving (Eq, Show)

extractWith :: String -> String -> Maybe String
extractWith style = fmap packagesToNix . traverse f . parseLisp
  where
    f = case style of
      "setup" -> extract_setup_el
      "use-package" -> extract_use_package
      _ -> const Nothing

extractFor :: String -> LispVal -> Maybe Package
extractFor k (List (Atom k' : Atom pkg : _)) =
  if k == k' then Just $ Package pkg else Nothing
extractFor _ _ = Nothing

extract_setup_el = extractFor "setup"

extract_use_package = extractFor "use-package"

packagesToNix :: [Package] -> String
packagesToNix pkgs = "[" <> unwords (fmap fromPackage pkgs) <> "]"
