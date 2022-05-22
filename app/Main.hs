{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import Lisp
import Data.List
import Data.Maybe
import System.Directory
import Control.Monad
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Unable to parse lisp"
    (x:_) -> do
      e <- doesFileExist x
      when e $ do
        src <- readFile x
        let pkgs = extract_packages src
        print pkgs

extract_packages :: String -> String
extract_packages = packagesToNix . fromMaybe mempty . traverse extract_setup_el . parseLisp

-- Nix --------------------------------------------------
newtype Package = Package { fromPackage :: String }
  deriving (Eq, Show)

extract_setup_el :: LispVal -> Maybe Package
extract_setup_el (List (Atom "setup":rest)) =
  case rest of
    (Atom pkg:_) -> Just $ Package pkg
    (List (Atom "quote":Atom pkg:_):_) -> Just $ Package pkg
extract_setup_el _ = Nothing

packagesToNix :: [Package] -> String
packagesToNix pkgs = "[" <> unwords (fmap fromPackage pkgs) <> "]"

