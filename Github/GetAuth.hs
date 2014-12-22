{-# LANGUAGE CPP #-}

module Github.GetAuth where

import Utility.Env

#if MIN_VERSION_github(0,9,0)
import qualified Github.Auth as Github
#else
import qualified Github.Issues as Github
#endif
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T

getAuth :: IO (Maybe Github.GithubAuth)
getAuth = do
	user <- getEnv "GITHUB_USER"
	password <- getEnv "GITHUB_PASSWORD"
	return $ case (user, password) of
		(Just u, Just p) -> Just $ 
			Github.GithubBasicAuth (tobs u) (tobs p)
		_ -> Nothing
  where
	tobs = encodeUtf8 . T.pack
