{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import Lib
import Lisp
import Data.List
import Data.Maybe

main :: IO ()
main = interact $ packagesToNix
                  . fromMaybe mempty
                  . traverse extract_setup_el
                  . parseLisp

-- Nix --------------------------------------------------
newtype Package = Package { fromPackage :: String }
  deriving (Eq, Show)

extract_setup_el :: LispVal -> Maybe Package
extract_setup_el (List (Atom "setup":Atom pkg:_)) = Just $ Package pkg
extract_setup_el _ = Nothing

packagesToNix :: [Package] -> String
packagesToNix pkgs = "[" <> unwords (fmap fromPackage pkgs) <> "]"

