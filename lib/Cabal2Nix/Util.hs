{-# LANGUAGE OverloadedStrings #-}
module Cabal2Nix.Util where

import System.Directory
import System.FilePath

import Control.Monad
import Data.String (IsString)

import Data.ByteString.Char8 (pack, unpack)
import Data.Text (Text)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as Base16

import Data.List.NonEmpty (NonEmpty)
import Data.Fix(Fix(..))
import Nix.Expr

listDirectories :: FilePath -> IO [FilePath]
listDirectories p =
  filter (/= ".git") <$> listDirectory p
  >>= filterM (doesDirectoryExist . (p </>))

quoted :: (IsString a, Semigroup a) => a -> a
quoted str = "\"" <> str <> "\""

selectOrThrow :: NExpr -> NAttrPath NExpr -> Text -> NExpr
selectOrThrow obj path msg = Fix (NSelect obj path (Just $ mkThrow msg))

mkThrow :: Text -> NExpr
mkThrow msg = (mkSym "builtins" @. "throw") @@ mkStr msg

sha256 :: String -> String
sha256 = unpack . Base16.encode . hash . pack

bindPath :: NonEmpty Text -> NExpr -> Binding NExpr
bindPath ks e = NamedVar (fmap StaticKey ks) e nullPos
