module Github.EnumRepos where

import qualified Github.Repos as Github
import Data.List
import Data.List.Utils
import Data.Maybe

import Utility.PartialPrelude
import qualified Git
import qualified Git.Types

-- A github user and repo.
data GithubUserRepo = GithubUserRepo String String
	deriving (Eq, Show, Read, Ord)

class ToGithubUserRepo a where
	toGithubUserRepo :: a -> GithubUserRepo

instance ToGithubUserRepo Github.Repo where
	toGithubUserRepo r = GithubUserRepo 
		(Github.githubOwnerLogin $ Github.repoOwner r)
		(Github.repoName r)

instance ToGithubUserRepo Github.RepoRef where
	toGithubUserRepo (Github.RepoRef owner name) = 
		GithubUserRepo (Github.githubOwnerLogin owner) name

gitHubRepos :: Git.Repo -> [Git.Repo]
gitHubRepos = fst . unzip . gitHubPairs

gitHubRemotes :: Git.Repo -> [GithubUserRepo]
gitHubRemotes = snd . unzip . gitHubPairs

gitHubPairs :: Git.Repo -> [(Git.Repo, GithubUserRepo)]
gitHubPairs = filter (not . wiki ) . mapMaybe check . Git.Types.remotes
  where
	check r@Git.Repo { Git.Types.location = Git.Types.Url u } =
		headMaybe $ mapMaybe (checkurl r $ show u) gitHubUrlPrefixes
	check _ = Nothing
	checkurl r u prefix
		| prefix `isPrefixOf` u && length bits == 2 =
			Just (r,
				GithubUserRepo (bits !! 0)
					(dropdotgit $ bits !! 1))
		| otherwise = Nothing
	  where
		rest = drop (length prefix) u
		bits = filter (not . null) $ split "/" rest
	dropdotgit s
		| ".git" `isSuffixOf` s = take (length s - length ".git") s
		| otherwise = s
	wiki (_, GithubUserRepo _ u) = ".wiki" `isSuffixOf` u

{- All known prefixes for urls to github repos. -}
gitHubUrlPrefixes :: [String]
gitHubUrlPrefixes = 
	[ "git@github.com:"
	, "git://github.com/"
	, "https://github.com/"
	, "http://github.com/"
	, "ssh://git@github.com/~/"
	]

