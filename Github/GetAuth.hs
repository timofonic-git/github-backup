module Github.GetAuth where

import Utility.Env
import Utility.Monad

import qualified GitHub.Auth as Github
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as B

getAuth :: IO (Maybe Github.Auth)
getAuth = getM id
	[ do
		user <- getEnv "GITHUB_USER"
		password <- getEnv "GITHUB_PASSWORD"
		return $ case (user, password) of
			(Just u, Just p) -> Just $ 
				Github.BasicAuth (tobs u) (tobs p)
			_ -> Nothing
	, do
		oauthtoken <- getEnv "GITHUB_OAUTH_TOKEN"
		return $ case oauthtoken of
			Just t -> Just $ Github.OAuth (B.fromString t)
			Nothing -> Nothing
	]
  where
	tobs = encodeUtf8 . T.pack
