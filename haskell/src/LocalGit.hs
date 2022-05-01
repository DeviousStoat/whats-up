module LocalGit where

import Control.Applicative
import Control.Monad (liftM2)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (withCurrentDirectory)
import System.Process (readProcess)

gitProcess :: [String] -> IO Text
gitProcess args = T.pack <$> readProcess "git" args []

sortedGitTags :: IO Text
sortedGitTags = gitProcess ["tag", "--sort=-v:refname"]

gitRemoteUrl :: IO Text
gitRemoteUrl = gitProcess ["remote", "get-url", "origin"]

gitCommitsBetween :: Text -> Text -> IO Text
gitCommitsBetween tag branch =
  gitProcess ["log", "--pretty=format:%s", T.unpack (tag <> ".." <> branch)]

sortedGitTagsInProj :: FilePath -> IO Text
sortedGitTagsInProj proj = withCurrentDirectory proj sortedGitTags

gitRemoteUrlInProj :: FilePath -> IO Text
gitRemoteUrlInProj proj = withCurrentDirectory proj gitRemoteUrl

gitCommitsBetweenInProj :: FilePath -> Text -> Text -> IO Text
gitCommitsBetweenInProj proj tag branch =
  withCurrentDirectory proj $ gitCommitsBetween tag branch

latestTagInProj :: FilePath -> IO Text
latestTagInProj = fmap getLatest . sortedGitTagsInProj
  where
    getLatest = head . T.lines

-- Gets the organisation and the repo from a git url (https or ssh)
-- "https://github.com/my_org/my_repo.git" -> ("my_org", "my_repo")
-- "git@github.com:my_org/my_repo.git" -> ("my_org", "my_repo")
orgRepoFromGitUrl :: Text -> (Text, Text)
orgRepoFromGitUrl url = liftA2 (,) getOrg getRepo url
  where
    -- TODO: these could probably be better
    getOrg =
      if "https" `T.isPrefixOf` url
        then T.takeWhile (/= '/') . dropWhileIncl (/= '/') . stripHttps
        else T.takeWhile (/= '/') . dropWhileIncl (/= ':')
    getRepo = T.takeWhile (/= '.') . T.takeWhileEnd (/= '/')

    stripHttps = fromMaybe url . T.stripPrefix "https://"
    dropWhileIncl f = T.tail . T.dropWhile f

gitOrgRepo :: FilePath -> IO (Text, Text)
gitOrgRepo = fmap orgRepoFromGitUrl . gitRemoteUrlInProj

prNumbersFromCommits :: Text -> [Integer]
prNumbersFromCommits =
  fmap (read . T.unpack . T.takeWhile (/= ' ') . T.tail . T.dropWhile (/= '#'))
    . filter isMergeCommit
    . T.lines
  where
    isMergeCommit =
      liftM2
        (&&)
        ("Merge pull request #" `T.isPrefixOf`)
        (not . ("dependabot" `T.isInfixOf`))

gitPRNumbersBetween :: FilePath -> Text -> Text -> IO [Integer]
gitPRNumbersBetween proj tag branch =
  gitCommitsBetweenInProj proj tag branch <&> prNumbersFromCommits
