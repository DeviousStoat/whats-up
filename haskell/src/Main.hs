module Main where

import Control.Concurrent.Async
import Data.Text hiding (head)
import GHApi
import LocalGit
import Options.Applicative
import System.Environment (getEnv)
import Template

data BaseParams = BaseParams
  { project :: FilePath,
    output :: FilePath,
    template :: FilePath,
    branch :: Text,
    verbose :: Bool
  }
  deriving (Show)

data Params = Params
  { baseParams :: BaseParams,
    tag :: Text,
    ghToken :: Text
  }
  deriving (Show)

mkTagParam :: Parser (Maybe Text)
mkTagParam =
  optional
    ( strOption $
        long "tag"
          <> short 'g'
          <> metavar "TAG"
          <> help "Tag from which to gather PRs (default: latest tag)"
    )

mkGHTokenParam :: Parser (Maybe Text)
mkGHTokenParam =
  optional
    ( strOption
        ( long "gh-token"
            <> short 'k'
            <> metavar "TOKEN"
            <> help
              "Github token to use to communicate with the API \
              \(default: env var GH_TOKEN)"
        )
    )

mkBaseParams :: Parser BaseParams
mkBaseParams =
  BaseParams
    <$> strOption
      ( long "project"
          <> short 'p'
          <> metavar "PROJECT"
          <> value "."
          <> showDefault
          <> help "Path to the git project"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT"
          <> value "helease_notes.md"
          <> showDefault
          <> help "Path to output file"
      )
    <*> strOption
      ( long "template"
          <> short 't'
          <> metavar "TEMPLATE"
          <> value "helease_notes.tmpl"
          <> showDefault
          <> help "Path to the template file"
      )
    <*> strOption
      ( long "branch"
          <> short 'b'
          <> metavar "BRANCH"
          <> value "develop"
          <> showDefault
          <> help "Branch that will be released"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Want me to talk?"
      )

mkEnhancedBaseParams :: Parser (BaseParams, Maybe Text, Maybe Text)
mkEnhancedBaseParams = (,,) <$> mkBaseParams <*> mkTagParam <*> mkGHTokenParam

-- TODO: this feels ugly, look into optparse
mkParams :: (BaseParams, Maybe Text, Maybe Text) -> IO Params
mkParams (p@BaseParams {project = projPath}, mtag, mtoken) = case (mtag, mtoken) of
  (Just tag', Just token') -> return $ Params p tag' token'
  (Nothing, Just token') -> Params p <$> getTag <*> pure token'
  (Just tag', Nothing) -> Params p tag' <$> getToken
  (Nothing, Nothing) -> Params p <$> getTag <*> getToken
  where
    getTag = latestTagInProj projPath
    getToken = pack <$> getEnv "GH_TOKEN"

run :: Params -> IO ()
run params = do
  let projPath = (project . baseParams) params
  tmpl <- readFile (template . baseParams $ params)
  (org, repo) <- gitOrgRepo $ (project . baseParams) params

  prNbs <- gitPRNumbersBetween projPath (tag params) ((branch . baseParams) params)
  prs <- mapConcurrently (getPR org repo (ghToken params)) prNbs
  writeFile (output . baseParams $ params) ""
  mapM_ (writePrToFile (output . baseParams $ params) (pack tmpl)) prs

main :: IO ()
main = execParser opts >>= mkParams >>= run
  where
    opts =
      info (mkEnhancedBaseParams <**> helper) $
        fullDesc
          <> progDesc "Generate release notes"
          <> header "helease - Release notes generator"
