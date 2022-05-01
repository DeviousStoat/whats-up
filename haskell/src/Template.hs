module Template where

import GHApi
import Data.Text (Text)
import qualified Data.Text as T

supportedKeywords :: [Text]
supportedKeywords =
  ["@title", "@url", "@user_login", "@number", "@body"]

prToTemplate :: Text -> PR -> Text
prToTemplate template pr = foldl replaceKw template supportedKeywords
  where
    replaceKw tmpl kw = case kw of
      "@title" -> T.replace kw (title pr) tmpl
      "@url" -> T.replace kw (url pr) tmpl
      "@user_login" -> T.replace kw (login . user $ pr) tmpl
      "@number" -> T.replace kw (T.pack . show . number $ pr) tmpl
      "@body" -> T.replace kw (bodyValue . body $ pr) tmpl
      _ -> undefined

writePrToFile :: FilePath -> Text -> PR -> IO ()
writePrToFile output template pr =
  appendFile output (T.unpack $ prToTemplate template pr)
