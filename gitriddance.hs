{- gitriddance - close all open issues and pull requests
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Main where

import qualified GitHub.Endpoints.Repos as Github
import qualified GitHub.Endpoints.Issues as Github
import qualified GitHub.Endpoints.Issues.Comments as Github
import qualified GitHub.Data.Id as Github
import System.Environment
import Data.String
import qualified Data.Text as T

import Common
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

closeall :: Github.Auth -> GithubUserRepo -> String -> IO ()
closeall auth (GithubUserRepo user repo) msg =
	either (oops "getting issue list") (mapM_ close)
		=<< Github.issuesForRepo' (Just auth) (fromString user) (fromString repo) Github.stateOpen
  where
	oops action err = error $ "failed " ++ action ++ ": " ++ show err
	close issue = do
		let i = Github.issueNumber issue
		putStrLn $ "closing issue: " ++ T.unpack (Github.issueTitle issue)
		either (oops "posting comment") (const $ return ())
			=<< Github.createComment auth (fromString user) (fromString repo) (Github.mkId (Github.Id 0) i) (T.pack msg)
		either (oops "closing issue/pull") (const $ return ())
			=<< Github.editIssue auth (fromString user) (fromString repo) (Github.mkId (Github.Id 0) i)
				(Github.editOfIssue { Github.editIssueState = Just Github.StateClosed } )
