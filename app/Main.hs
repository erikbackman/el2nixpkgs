{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Lisp
import Data.List
import Data.Maybe
import System.Directory
import Control.Monad
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    []               -> putStrLn "Unable to parse lisp"
    (style:config:_) -> do
      readable <- doesFileExist config
      when readable $ do
        src <- readFile' config
        putStrLn $ maybe "Unsupported" show (extractWith style src)

extractWith :: String -> String -> Maybe String
extractWith "setup" lisp = extract_packages lisp extract_setup_el
extractWith _      _     = Nothing

extract_packages :: String -> (LispVal -> Maybe Package) -> Maybe String
extract_packages src extract = fmap packagesToNix . traverse extract . parseLisp $ src

newtype Package = Package { fromPackage :: String }
  deriving (Eq, Show)

extract_setup_el :: LispVal -> Maybe Package
extract_setup_el (List (Atom "setup":Atom pkg:_)) = Just $ Package pkg
extract_setup_el _ = Nothing

packagesToNix :: [Package] -> String
packagesToNix pkgs = "[" <> unwords (fmap fromPackage pkgs) <> "]"

