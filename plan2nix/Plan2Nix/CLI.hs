module Plan2Nix.CLI
  ( Args(..)
  , parsePlan2NixArgs
  ) where

import Options.Applicative hiding (option)
import Data.Semigroup ((<>))


--------------------------------------------------------------------------------
-- CLI Arguments
data Args = Args
  { argOutputDir :: FilePath
  , argPlanJSON :: FilePath
  , argCacheFile :: FilePath
  } deriving Show

-- Argument Parser
args :: Parser Args
args = Args
  <$> strOption ( long "output" <> short 'o' <> metavar "DIR" <> help "Generate output in DIR" )
  <*> strOption ( long "plan-json" <> value "dist-newstyle/cache/plan.json" <> showDefault <> metavar "FILE" <> help "Override project plan.json" )
  <*> strOption ( long "cache" <> value ".nix-tools.cache" <> showDefault <> metavar "FILE" <> help "Dependency cache file" )

parsePlan2NixArgs :: IO Args
parsePlan2NixArgs = execParser opts
  where opts = info (args <**> helper)
          ( fullDesc
         <> progDesc "Generate a Nix expression for a Haskell package using Cabal"
         <> header "plan-to-nix - a stack to nix converter" )
