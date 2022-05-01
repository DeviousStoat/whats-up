{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module GHApi where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.HTTP.Req

newtype User = User {login :: Text}
  deriving (Show, Generic, FromJSON)

newtype Body = Body {bodyValue :: Text}
  deriving (Show, Generic)

instance FromJSON Body where
  parseJSON Null = return $ Body ""
  parseJSON (String s) = return $ Body s
  parseJSON invalid =
    prependFailure "parsing body failed, " (typeMismatch "String or Null" invalid)

data PR = PR
  { title :: Text,
    number :: Integer,
    url :: Text,
    user :: User,
    body :: Body
  }
  deriving (Show, Generic, FromJSON)

reqParams :: Text -> Option scheme
reqParams ghToken =
  mconcat
    [ header "Authorization" (encodeUtf8 $ "Bearer " <> ghToken),
      header "Accept" "application/vnd.github.v3+json",
      header "User-Agent" "helease-app"
    ]

getPR :: Text -> Text -> Text -> Integer -> IO PR
getPR org repo ghToken prNb = responseBody <$> runReq defaultHttpConfig request
  where
    url' = https "api.github.com" /: "repos" /: org /: repo /: "pulls" /: toText prNb
    request = req GET url' NoReqBody jsonResponse (reqParams ghToken)
    toText = pack . show
