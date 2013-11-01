{- github-backup
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Monoid
import System.Environment
import Control.Exception (try, SomeException)
import Text.Show.Pretty
import "mtl" Control.Monad.State.Strict
import qualified Github.Data.Readable as Github
import qualified Github.Repos as Github
import qualified Github.Repos.Forks as Github
import qualified Github.PullRequests as Github
import qualified Github.Repos.Watching as Github
import qualified Github.Data.Definitions as Github ()
import qualified Github.Issues as Github
import qualified Github.Issues.Comments
import qualified Github.Issues.Milestones

import Common
import Utility.State
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Types
import qualified Git.Command
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.UpdateIndex
import Git.HashObject
import Git.FilePath

-- A github user and repo.
data GithubUserRepo = GithubUserRepo String String
	deriving (Eq, Show, Read, Ord)

toGithubUserRepo :: Github.Repo -> GithubUserRepo
toGithubUserRepo r = GithubUserRepo 
	(Github.githubOwnerLogin $ Github.repoOwner r)
	(Github.repoName r)

repoUrl :: GithubUserRepo -> String
repoUrl (GithubUserRepo user remote) =
	"git://github.com/" ++ user ++ "/" ++ remote ++ ".git"

repoWikiUrl :: GithubUserRepo -> String
repoWikiUrl (GithubUserRepo user remote) =
	"git://github.com/" ++ user ++ "/" ++ remote ++ ".wiki.git"

-- A name for a github api call.
type ApiName = String

-- A request to make of github. It may have an extra parameter.
data Request = RequestSimple ApiName GithubUserRepo
	| RequestNum ApiName GithubUserRepo Int
	deriving (Eq, Show, Read, Ord)

requestRepo :: Request -> GithubUserRepo
requestRepo (RequestSimple _ repo) = repo
requestRepo (RequestNum _ repo _) = repo

requestName :: Request -> String
requestName (RequestSimple name _) = name
requestName (RequestNum name _ _) = name

data BackupState = BackupState
	{ failedRequests :: S.Set Request
	, retriedRequests :: S.Set Request
	, gitRepo :: Git.Repo
	}

{- Our monad. -}
newtype Backup a = Backup { runBackup :: StateT BackupState IO a }
	deriving (
		Monad,
		MonadState BackupState,
		MonadIO,
		Functor,
		Applicative
	)

inRepo :: (Git.Repo -> IO a) -> Backup a
inRepo a = liftIO . a =<< getState gitRepo

failedRequest :: Request -> Github.Error-> Backup ()
failedRequest req e = unless ignorable $ do
	set <- getState failedRequests
	changeState $ \s -> s { failedRequests = S.insert req set }
  where
	-- "410 Gone" is used for repos that have issues etc disabled.
	ignorable = "410 Gone" `isInfixOf` show e

runRequest :: Request -> Backup ()
runRequest req = do
	-- avoid re-running requests that were already retried
	retried <- getState retriedRequests
	unless (S.member req retried) $
		(lookupApi req) req

type Storer = Request -> Backup ()
data ApiListItem = ApiListItem ApiName Storer Bool
apiList :: [ApiListItem]
apiList = 
	[ ApiListItem "userrepo" userrepoStore True
	, ApiListItem "watchers" watchersStore True
	, ApiListItem "pullrequests" pullrequestsStore True
	, ApiListItem "pullrequest" pullrequestStore False
	, ApiListItem "milestones" milestonesStore True
	, ApiListItem "issues" issuesStore True
	, ApiListItem "issuecomments" issuecommentsStore False
	-- comes last because it recurses on to the forks
	, ApiListItem "forks" forksStore True
	]

{- Map of Github api calls we can make to store their data. -}
api :: M.Map ApiName Storer
api = M.fromList $ map (\(ApiListItem n s _) -> (n, s)) apiList

{- List of toplevel api calls that are followed to get all data. -}
toplevelApi :: [ApiName]
toplevelApi = map (\(ApiListItem n _ _) -> n) $
	filter (\(ApiListItem _ _ toplevel) -> toplevel) apiList

lookupApi :: Request -> Storer
lookupApi req = fromMaybe bad $ M.lookup name api
  where
	name = requestName req
	bad = error $ "internal error: bad api call: " ++ name

userrepoStore :: Storer
userrepoStore = simpleHelper Github.userRepo $ \req r -> do
	when (Github.repoHasWiki r == Just True) $
		updateWiki $ toGithubUserRepo r
	store "repo" req r

watchersStore :: Storer
watchersStore = simpleHelper Github.watchersFor $ storeSorted "watchers"

pullrequestsStore :: Storer
pullrequestsStore = simpleHelper Github.pullRequestsFor $
	forValues $ \req r -> do
		let repo = requestRepo req
		let n = Github.pullRequestNumber r
		runRequest $ RequestNum "pullrequest" repo n

pullrequestStore :: Storer
pullrequestStore = numHelper Github.pullRequest $ \n ->
	store ("pullrequest" </> show n)

milestonesStore :: Storer
milestonesStore = simpleHelper Github.Issues.Milestones.milestones $
	forValues $ \req m -> do
		let n = Github.milestoneNumber m
		store ("milestone" </> show n) req m

issuesStore :: Storer
issuesStore = withHelper (\u r y ->
	Github.issuesForRepo u r (y <> [Github.Open])
		>>= either (return . Left)
			(\xs -> Github.issuesForRepo u r
				(y <> [Github.OnlyClosed])
					>>= either (return . Left)
						(\ys -> return (Right (xs <> ys)))))
	[Github.PerPage 100] go
  where
	go = forValues $ \req i -> do
		let repo = requestRepo req
		let n = Github.issueNumber i
		store ("issue" </> show n) req i
		runRequest (RequestNum "issuecomments" repo n)

issuecommentsStore :: Storer
issuecommentsStore = numHelper Github.Issues.Comments.comments $ \n ->
	forValues $ \req c -> do
		let i = Github.issueCommentId c
		store ("issue" </> show n ++ "_comment" </> show i) req c

forksStore :: Storer
forksStore = simpleHelper Github.forksFor $ \req fs -> do
	storeSorted "forks" req fs
	mapM_ (traverse . toGithubUserRepo) fs
  where
	traverse fork = whenM (addFork fork) $
		gatherMetaData fork

forValues :: (Request -> v -> Backup ()) -> Request -> [v] -> Backup ()
forValues handle req vs = forM_ vs (handle req)

type ApiCall v = String -> String -> IO (Either Github.Error v)
type ApiWith v b = String -> String -> b -> IO (Either Github.Error v)
type ApiNum v = ApiWith v Int
type Handler v = Request -> v -> Backup ()
type Helper = Request -> Backup ()

simpleHelper :: ApiCall v -> Handler v -> Helper
simpleHelper call handle req@(RequestSimple _ (GithubUserRepo user repo)) =
	either (failedRequest req) (handle req) =<< liftIO (call user repo)
simpleHelper _ _ r = badRequest r

withHelper :: ApiWith v b -> b -> Handler v -> Helper
withHelper call b handle req@(RequestSimple _ (GithubUserRepo user repo)) =
	either (failedRequest req) (handle req) =<< liftIO (call user repo b)
withHelper _ _ _ r = badRequest r

numHelper :: ApiNum v -> (Int -> Handler v) -> Helper
numHelper call handle req@(RequestNum _ (GithubUserRepo user repo) num) =
	either (failedRequest req) (handle num req) =<< liftIO (call user repo num)
numHelper _ _ r = badRequest r

badRequest :: Request -> a
badRequest r = error $ "internal error: bad request type " ++ show r

store :: Show a => FilePath -> Request -> a -> Backup ()
store filebase req val = do
	file <- location (requestRepo req) <$> workDir
	liftIO $ do
		createDirectoryIfMissing True (parentDir file)
		writeFile file (ppShow val)
  where
	location (GithubUserRepo user repo) workdir =
		workdir </> user ++ "_" ++ repo </> filebase

workDir :: Backup FilePath
workDir = (</>)
		<$> (Git.repoPath <$> getState gitRepo)
		<*> pure "github-backup.tmp"

storeSorted :: Ord a => Show a => FilePath -> Request -> [a] -> Backup ()
storeSorted file req val = store file req (sort val)

gitHubRepos :: Backup [Git.Repo]
gitHubRepos = fst . unzip . gitHubPairs <$> getState gitRepo

gitHubRemotes :: Backup [GithubUserRepo]
gitHubRemotes = snd . unzip . gitHubPairs <$> getState gitRepo

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
		bits = split "/" rest
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

{- Commits all files in the workDir into the github branch, and deletes the
 - workDir.
 -
 - The commit is made to the github branch without ever checking it out,
 - or otherwise disturbing the work tree.
 -}
commitWorkDir :: Backup ()
commitWorkDir = do
	dir <- workDir
	whenM (liftIO $ doesDirectoryExist dir) $ do
		branchref <- getBranch
		withIndex $ do
			r <- getState gitRepo
			liftIO $ do
				-- Reset index to current content of github
				-- branch. Does not touch work tree.
				Git.Command.run
					[Param "reset", Param "-q", Param $ show branchref, File "." ] r
				-- Stage workDir files into the index.
				h <- hashObjectStart r
				Git.UpdateIndex.streamUpdateIndex r
					[genstream r dir h]
				hashObjectStop h
				-- Commit
				void $ Git.Branch.commit "github-backup" fullname [branchref] r
				removeDirectoryRecursive dir
  where
  	genstream r dir h streamer = do
		fs <- filter (not . dirCruft) <$> dirContentsRecursive dir
		forM_ fs $ \f -> do
			let basefile = joinPath $ drop 1 $ splitPath f
			sha <- hashFile h f
			path <- toTopFilePath basefile r
			streamer $ Git.UpdateIndex.updateIndexLine
				sha Git.Types.FileBlob path

{- Returns the ref of the github branch, creating it first if necessary. -}
getBranch :: Backup Git.Ref
getBranch = maybe (hasOrigin >>= create) return =<< branchsha
  where
  	create True = do
		inRepo $ Git.Command.run
			[Param "branch", Param $ show branchname, Param $ show originname]
		fromMaybe (error $ "failed to create " ++ show branchname)
			<$> branchsha
	create False = withIndex $
		inRepo $ Git.Branch.commit "branch created" fullname []
	branchsha = inRepo $ Git.Ref.sha fullname

{- Runs an action with a different index file, used for the github branch. -}
withIndex :: Backup a -> Backup a
withIndex a = do
	r <- getState gitRepo
	let f = Git.localGitDir r </> "github-backup.index"
	e <- liftIO getEnvironment
	let r' = r { Git.Types.gitEnv = Just $ ("GIT_INDEX_FILE", f):e }
	changeState $ \s -> s { gitRepo = r' }
	v <- a
	changeState $ \s -> s { gitRepo = (gitRepo s) { Git.Types.gitEnv = Git.Types.gitEnv r } }
	return v

branchname :: Git.Ref
branchname = Git.Ref "github"

fullname :: Git.Ref
fullname = Git.Ref $ "refs/heads/" ++ show branchname

originname :: Git.Ref
originname = Git.Ref $ "refs/remotes/origin/" ++ show branchname

hasOrigin :: Backup Bool
hasOrigin = inRepo $ Git.Ref.exists originname

updateWiki :: GithubUserRepo -> Backup ()
updateWiki fork =
	ifM (any (\r -> Git.remoteName r == Just remote) <$> remotes)
		( void fetchwiki
		, void $
			-- github often does not really have a wiki,
			-- don't bloat config if there is none
			unlessM (addRemote remote $ repoWikiUrl fork) $
				removeRemote remote
		)
  where
	fetchwiki = inRepo $ Git.Command.runBool [Param "fetch", Param remote]
	remotes = Git.remotes <$> getState gitRepo
	remote = remoteFor fork
	remoteFor (GithubUserRepo user repo) =
		"github_" ++ user ++ "_" ++ repo ++ ".wiki"

addFork :: GithubUserRepo -> Backup Bool
addFork fork =
	ifM (elem fork <$> gitHubRemotes)
		( return False
		, do
			liftIO $ putStrLn $ "New fork: " ++ repoUrl fork
			_ <- addRemote (remoteFor fork) (repoUrl fork)
			return True
		)
  where
	remoteFor (GithubUserRepo user repo) = "github_" ++ user ++ "_" ++ repo

{- Adds a remote, also fetching from it. -}
addRemote :: String -> String -> Backup Bool
addRemote remotename remoteurl =
	inRepo $ Git.Command.runBool
		[ Param "remote"
		, Param "add"
		, Param "-f"
		, Param remotename
		, Param remoteurl
		]

removeRemote :: String -> Backup ()
removeRemote remotename = void $ inRepo $ Git.Command.runBool
	[ Param "remote"
	, Param "rm"
	, Param remotename
	]

{- Fetches from the github remote. Done by github-backup, just because
 - it would be weird for a backup to not fetch all available data.
 - Even though its real focus is on metadata not stored in git. -}
fetchRepo :: Git.Repo -> Backup Bool
fetchRepo repo = inRepo $ Git.Command.runBool
	[Param "fetch", Param $ fromJust $ Git.Types.remoteName repo]

{- Gathers metadata for the repo. Retuns a list of files written
 - and a list that may contain requests that need to be retried later. -}
gatherMetaData :: GithubUserRepo -> Backup ()
gatherMetaData repo = do
	liftIO $ putStrLn $ "Gathering metadata for " ++ repoUrl repo ++ " ..."
	mapM_ call toplevelApi
  where
	call name = runRequest $ RequestSimple name repo

storeRetry :: [Request] -> Git.Repo -> IO ()
storeRetry [] r = void $ do
	try $ removeFile (retryFile r) :: IO (Either SomeException ()) 
storeRetry retryrequests r = writeFile (retryFile r) (show retryrequests)

loadRetry :: Git.Repo -> IO [Request]
loadRetry r = maybe [] (fromMaybe [] . readish)
	<$> catchMaybeIO (readFileStrict (retryFile r))

retryFile :: Git.Repo -> FilePath
retryFile r = Git.localGitDir r </> "github-backup.todo"

retry :: Backup (S.Set Request)
retry = do
	todo <- inRepo loadRetry
	unless (null todo) $ do
		liftIO $ putStrLn $
			"Retrying " ++ show (length todo) ++
			" requests that failed last time..."
		mapM_ runRequest todo
	retriedfailed <- getState failedRequests
	changeState $ \s -> s
		{ failedRequests = S.empty
		, retriedRequests = S.fromList todo
		}
	return retriedfailed

summarizeRequests :: [Request] -> [String]
summarizeRequests = go M.empty
  where
	go m [] = map format $ sort $ map swap $ M.toList m
	go m (r:rs) = go (M.insertWith (+) (requestName r) (1 :: Integer) m) rs
	format (num, name) = show num ++ "\t" ++ name
	swap (a, b) = (b, a)

{- Save all backup data. Files that were written to the workDir are committed.
 - Requests that failed are saved for next time. Requests that were retried
 - this time and failed are ordered last, to ensure that we don't get stuck
 - retrying the same requests and not making progress when run again.
 -}
save :: S.Set Request -> Backup ()
save retriedfailed = do
	commitWorkDir
	failed <- getState failedRequests
	let toretry = S.toList failed ++ S.toList retriedfailed
	inRepo $ storeRetry toretry
	unless (null toretry) $
		error $ unlines $
			["Backup may be incomplete; " ++ 
				show (length toretry) ++ " requests failed:"
			] ++ map ("  " ++) (summarizeRequests toretry) ++ 
			[ "Run again later."
			]

newState :: Git.Repo -> BackupState
newState = BackupState S.empty S.empty

backupRepo :: (Maybe Git.Repo) -> IO ()
backupRepo Nothing = error "not in a git repository, and nothing specified to back up"
backupRepo (Just repo) = evalStateT (runBackup go) . newState =<< Git.Config.read repo
  where
	go = do
		retriedfailed <- retry
		remotes <- gitHubPairs <$> getState gitRepo
		when (null remotes) $
			error "no github remotes found"
		forM_ remotes $ \(r, remote) -> do
			_ <- fetchRepo r
			gatherMetaData remote
		save retriedfailed

backupName :: String -> IO ()
backupName name = do
	l <- sequence
	 	[ Github.userRepos name Github.All
		, Github.reposWatchedBy name
		, Github.organizationRepos name
		]
	let repos = concat $ rights l
	when (null repos) $
		if (null $ rights l)
			then error $ unlines $ "Failed to query github for repos:" : map show (lefts l)
			else error $ "No GitHub repositories found for " ++ name
	status <- forM repos $ \repo -> do
		let dir = Github.repoName repo
		unlessM (doesDirectoryExist dir) $ do
			putStrLn $ "New repository: " ++ dir
			ok <- boolSystem "git"
				[ Param "clone"
				, Param (Github.repoGitUrl repo)
				, Param dir
				]
			unless ok $ error "clone failed"
		try (backupRepo . Just =<< Git.Construct.fromPath dir)
			:: IO (Either SomeException ())
	unless (null $ lefts status) $
		error "Failed to successfully back everything up. Run again later."

usage :: String
usage = "usage: github-backup [username|organization]"

main :: IO ()
main = getArgs >>= go
  where
	go (('-':_):_) = error usage
	go [] = backupRepo =<< Git.Construct.fromCwd
	go (name:[]) = backupName name
	go _= error usage

