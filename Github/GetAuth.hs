module Github.GetAuth where

import Utility.Env

import qualified GitHub.Auth as Github
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

getAuth :: IO (Maybe Github.Auth)
getAuth = do
	user <- getEnv "GITHUB_USER"
	password <- getEnv "GITHUB_PASSWORD"
	return $ case (user, password) of
		(Just u, Just p) -> Just $ 
			Github.BasicAuth (tobs u) (tobs p)
		_ -> Nothing
  where
	tobs = encodeUtf8 . T.pack
