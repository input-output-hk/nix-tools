{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cabal2Nix.Plan
where

import           Cabal2Nix.Util                           ( quoted
                                                          , bindPath
                                                          )
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.List.NonEmpty                       ( NonEmpty (..) )
import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import           Nix.Expr
import Distribution.Simple.Utils (shortRelativePath)

import Nix.Pretty (prettyNix)

import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)

import Cabal2Nix hiding (Git)
import qualified Cabal2Nix as C2N
import Cabal2Nix.Util

import Distribution.Nixpkgs.Fetch

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List (isSuffixOf, isInfixOf, isPrefixOf)
import Control.Monad.IO.Class

import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.Trans.Maybe

import qualified Hpack
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack

import System.IO
import Data.String (fromString)

import Control.Exception (catch, SomeException(..))


type Version = Text
type Revision = Text -- Can be: rNUM, cabal file sha256, or "default"
-- See stack2nix
type URL = String
type Rev = String

data Location
  = Git URL Rev
  | HG  URL Rev
  deriving (Show)

data Plan = Plan
  { packages :: HashMap Text (Maybe Package)
  , overlays :: HashMap Text (Maybe Package)
  , compilerVersion :: Text
  , compilerPackages :: HashMap Text (Maybe Version)
  } deriving (Show)


data PkgSrc
  = LocalPath Text -- ^ some local package (potentially overriding a package in the index as well)
  | DVCS Location [FilePath] -- ^ One or more packages fetched from git or similar
  deriving Show

data Package = Package
  { packageVersion :: Version
  , packageRevision :: Maybe Revision
  , packageFlags :: HashMap Text Bool
  , packageSrc :: Maybe PkgSrc
  } deriving (Show)

--------------------------------------------------------------------------------
-- Copied from stack2nix/Main.hs
readCache :: IO [( String -- url
                 , String -- rev
                 , String -- subdir
                 , String -- sha256
                 , String -- pkgname
                 , String -- nixexpr-path
                 )]
readCache = fmap (toTuple . words) . lines <$> readFile ".stack-to-nix.cache"
  where toTuple [ url, rev, subdir, sha256, pkgname, exprPath ]
          = ( url, rev, subdir, sha256, pkgname, exprPath )

appendCache :: String -> String -> String -> String -> String -> String -> IO ()
appendCache url rev subdir sha256 pkgname exprPath =
  if (url /= "") then do
    appendFile ".stack-to-nix.cache" $ unwords [ url, rev, subdir, sha256, pkgname, exprPath ]
    appendFile ".stack-to-nix.cache" "\n"
  else return ()

cacheHits :: String -> String -> String -> IO [ (String, String) ]
cacheHits url rev subdir
  = do cache <- catch' readCache (const (pure []))
       return [ ( pkgname, exprPath )
              | ( url', rev', subdir', sha256, pkgname, exprPath ) <- cache
              , url == url'
              , rev == rev'
              , subdir == subdir' ]
  where catch' :: IO a -> (SomeException -> IO a) -> IO a
        catch' = catch

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc file doc =
  do handle <- openFile file WriteMode
     hPutDoc handle doc
     hClose handle

findCabalFiles :: FilePath -> IO [CabalFile]
findCabalFiles path = doesFileExist (path </> Hpack.packageConfig) >>= \case
  False -> fmap (OnDisk . (path </>)) . filter (isSuffixOf ".cabal") <$> listDirectory path
  True -> do
    mbPkg <- Hpack.readPackageConfig Hpack.defaultDecodeOptions {Hpack.decodeOptionsTarget = path </> Hpack.packageConfig}
    case mbPkg of
      Left e -> error e
      Right r ->
        return $ [InMemory (Just Hpack)
                           (Hpack.decodeResultCabalFile r)
                           (encodeUtf8 $ Hpack.renderPackage [] (Hpack.decodeResultPackage r))]

  where encodeUtf8 :: String -> ByteString
        encodeUtf8 = T.encodeUtf8 . T.pack
--------------------------------------------------------------------------------


plan2nix :: Plan -> IO NExpr
plan2nix (Plan { packages, overlays, compilerVersion, compilerPackages }) = do
  -- TODO: this is an aweful hack and expects plan-to-nix to be
  -- called from the toplevel project directory.
  cwd <- getCurrentDirectory
  overlay <- fmap (mkNonRecSet  . concat) . forM (Map.toList overlays) $ \case
    (name, Just (Package v r flags (Just (LocalPath folder)))) ->
      do cabalFiles <- findCabalFiles (T.unpack folder)
         forM cabalFiles $ \cabalFile ->
           let pkg = cabalFilePkgName cabalFile
               nix = ".plan.nix" </> pkg <.> "nix"
               nixFile = "nix" </> nix
               src = Just . C2N.Path $ relPath </> ".." </> (shortRelativePath cwd (T.unpack folder))
           in do createDirectoryIfMissing True (takeDirectory nixFile)
                 writeDoc nixFile =<<
                   prettyNix <$> cabal2nix src cabalFile
                 return $ fromString pkg $= mkPath False nix
    (name, Just (Package v r flags (Just (DVCS (Git url rev) subdirs)))) ->
      fmap concat . forM subdirs $ \subdir ->
      do cacheHits <- liftIO $ cacheHits url rev subdir
         case cacheHits of
           [] -> do
             fetch (\dir -> cabalFromPath url rev subdir $ dir </> subdir)
               (Source url rev UnknownHash subdir) >>= \case
               (Just (DerivationSource{..}, genBindings)) -> genBindings derivHash
               _ -> return []
           hits ->
             forM hits $ \( pkg, nix ) -> do
               return $ fromString pkg $= mkPath False nix
    _ -> return []

  return $ mkNonRecSet [
    "pkgs" $= ("hackage" ==> mkNonRecSet (
      [ "packages" $= (mkNonRecSet $ uncurry bind =<< Map.toList quotedPackages)
      , "compiler" $= mkNonRecSet
        [ "version" $= mkStr compilerVersion
        , "nix-name" $= mkStr ("ghc" <> Text.filter (/= '.') compilerVersion)
        , "packages" $= mkNonRecSet (fmap (uncurry bind') $ Map.toList $ mapKeys quoted compilerPackages)
        ]
      ]))
    , "overlay" $= ("hackage" ==> mkNonRecSet [ "packages" $= overlay ])
    ]
 where
  quotedPackages = mapKeys quoted packages
  bind pkg (Just (Package { packageVersion, packageRevision, packageFlags })) =
    let verExpr      = mkSym "hackage" @. pkg @. quoted packageVersion
        revExpr      = verExpr @. "revisions" @. maybe "default" quoted packageRevision
        flagBindings = Map.foldrWithKey
          (\fname val acc -> bindPath (pkg :| ["flags", fname]) (mkBool val) : acc)
          []
          packageFlags
    in  revBinding pkg revExpr : flagBindings
  bind pkg Nothing = [revBinding pkg mkNull]
  revBinding pkg revExpr = bindPath (pkg :| ["revision"]) revExpr
  bind' pkg ver = pkg $= maybe mkNull mkStr ver
  mapKeys f = Map.fromList . fmap (\(k, v) -> (f k, v)) . Map.toList

  relPath = "out" -- shortRelativePath (outputPath args) (dropFileName (stackFile args))
  cabalFromPath
          :: String    -- URL
          -> String    -- Revision
          -> FilePath  -- Subdir
          -> FilePath  -- Local Directory
          -> MaybeT IO (String -> IO [Binding NExpr])
  cabalFromPath url rev subdir path = do
          d <- liftIO $ doesDirectoryExist path
          unless d $ fail ("not a directory: " ++ path)
          cabalFiles <- liftIO $ findCabalFiles path
          return $ \sha256 ->
            forM cabalFiles $ \cabalFile -> do
            let pkg = cabalFilePkgName cabalFile
                nix = ".plan.nix" </> pkg <.> "nix"
                nixFile = "nix" </> nix -- outputPath args </> nix
                subdir' = if subdir == "." then Nothing
                          else Just subdir
                src = Just $ C2N.Git url rev (Just sha256) subdir'
            createDirectoryIfMissing True (takeDirectory nixFile)
            writeDoc nixFile =<<
              prettyNix <$> cabal2nix src cabalFile
            liftIO $ appendCache url rev subdir sha256 pkg nix
            return $ fromString pkg $= mkPath False nix
