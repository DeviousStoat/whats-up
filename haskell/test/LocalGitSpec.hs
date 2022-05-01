module LocalGitSpec (spec) where

import LocalGit
import Test.Hspec

spec :: Spec
spec = do
  describe "orgRepoFromGitUrl" $ do
    it "https url" $ do
      orgRepoFromGitUrl "https://github.com/my_org/my_repo.git"
        `shouldBe` ("my_org", "my_repo")
    it "ssh url" $ do
      orgRepoFromGitUrl "git@github.com:my_org/my_repo.git"
        `shouldBe` ("my_org", "my_repo")

  describe "prNumbersFromCommits" $ do
    it "no numbers" $ do
      prNumbersFromCommits "" `shouldBe` []
    it "some numbers" $ do
      prNumbersFromCommits
        "Merge pull request #05 from some guy\n\
        \Some commit\n\
        \Merge pull request #128 from some other guy"
        `shouldBe` [5, 128]
    it "filters dependabot prs" $ do
      prNumbersFromCommits
        "Merge pull request #05\n\
        \Some commit\n\
        \Merge pull request #07 from dependabot"
        `shouldBe` [5]
