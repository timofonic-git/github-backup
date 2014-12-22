{- gitriddance - close all open issues and pull requests
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Main where

import qualified Github.Repos as Github
#if MIN_VERSION_github(0,9,0)
import qualified Github.Auth as Github
#endif
import qualified Github.Issues as Github
import qualified Github.Issues.Comments as Github
import Data.Maybe
import Control.Applicative
import System.Environment

import Utility.PartialPrelude
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import Github.GetAuth
import Github.EnumRepos

main :: IO ()
main = do
	auth <- fromMaybe (error "Must set GITHUB_USER and GITHUB_PASSWORD")
		<$> getAuth
	r <- maybe (error "not in a git repository") Git.Config.read
		=<< Git.Construct.fromCwd
	msg <- maybe (getMsg r) id <$> (headMaybe <$> getArgs)
	case gitHubRemotes (onlyOriginRemote r) of
		[] -> error "origin does not seem to be a github repository"
		[origin] -> closeall auth origin msg
		_ -> error "somehow found multiple origin repos; this should be impossible!"

getMsg :: Git.Repo -> String
getMsg r = fromMaybe (error "core.gitriddance needs to be set to a message to use when closing issues/pull requests (or pass the message on the command line)")
	(Git.Config.getMaybe "core.gitriddance" r)

{- Limit to only having the origin remote; we don't want to affect any
 - other remotes that might be on github. -}
onlyOriginRemote :: Git.Repo -> Git.Repo
onlyOriginRemote r = r { Git.remotes = filter isorigin (Git.remotes r) }
  where
	isorigin rmt = Git.remoteName rmt == Just "origin"

closeall :: Github.GithubAuth -> GithubUserRepo -> String -> IO ()
closeall auth (GithubUserRepo user repo) msg =
	either (oops "getting issue list") (mapM_ close)
		=<< Github.issuesForRepo' (Just auth) user repo [Github.Open]
  where
	oops action err = error $ "failed " ++ action ++ ": " ++ show err
	close issue = do
		let i = Github.issueNumber issue
		putStrLn $ "closing issue: " ++ Github.issueTitle issue
		either (oops "posting comment") (const $ return ())
			=<< Github.createComment auth user repo i msg
		either (oops "closing issue/pull") (const $ return ())
			=<< Github.editIssue auth user repo i
				(Github.editOfIssue { Github.editIssueState = Just "closed" } )
