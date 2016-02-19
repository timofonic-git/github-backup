{- github-backup
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either
import Data.Monoid
import Options.Applicative
import Text.Show.Pretty
import "mtl" Control.Monad.State.Strict
import qualified Github.Repos as Github
#if MIN_VERSION_github(0,9,0)
import qualified Github.Auth as Github
#endif
import qualified Github.Repos.Forks as Github
import qualified Github.PullRequests as Github
import qualified Github.Repos.Watching as Github
import qualified Github.Repos.Starring as Github
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
import Github.GetAuth
import Github.EnumRepos
import Git.HashObject
import Git.FilePath
import Git.CatFile
import Utility.Env

repoUrl :: GithubUserRepo -> String
repoUrl (GithubUserRepo user remote) =
	"https://github.com/" ++ user ++ "/" ++ remote ++ ".git"

repoWikiUrl :: GithubUserRepo -> String
repoWikiUrl (GithubUserRepo user remote) =
	"https://github.com/" ++ user ++ "/" ++ remote ++ ".wiki.git"

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
	, retriedFailed :: S.Set Request
	, gitRepo :: Git.Repo
	, gitHubAuth :: Maybe Github.GithubAuth
	, deferredBackups :: [Backup ()]
	, catFileHandle :: Maybe CatFileHandle
	, noForks :: Bool
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

failedRequest :: Request -> Github.Error -> Backup ()
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
	[ ApiListItem "watchers" watchersStore True
	, ApiListItem "stargazers" stargazersStore True
	, ApiListItem "pullrequests" pullrequestsStore True
	, ApiListItem "pullrequest" pullrequestStore False
	, ApiListItem "milestones" milestonesStore True
	, ApiListItem "issues" issuesStore True
	, ApiListItem "issuecomments" issuecommentsStore False
	-- Recursive things last.
	, ApiListItem "userrepo" userrepoStore True
	, ApiListItem "forks" forksStore True
	]

{- Map of Github api calls we can make to store their data. -}
api :: M.Map ApiName Storer
api = M.fromList $ map (\(ApiListItem n s _) -> (n, s)) apiList

{- List of toplevel api calls that are followed to get data. -}
toplevelApi :: [ApiName]
toplevelApi = map (\(ApiListItem n _ _) -> n) $
	filter (\(ApiListItem _ _ toplevel) -> toplevel) apiList

lookupApi :: Request -> Storer
lookupApi req = fromMaybe bad $ M.lookup name api
  where
	name = requestName req
	bad = error $ "internal error: bad api call: " ++ name

watchersStore :: Storer
watchersStore = simpleHelper "watchers" Github.watchersFor' $
	storeSorted "watchers"

stargazersStore :: Storer
stargazersStore = simpleHelper "stargazers" Github.stargazersFor $
	storeSorted "stargazers"

pullrequestsStore :: Storer
pullrequestsStore = simpleHelper "pullrequest" Github.pullRequestsFor' $
	forValues $ \req r -> do
		let repo = requestRepo req
		let n = Github.pullRequestNumber r
		runRequest $ RequestNum "pullrequest" repo n

pullrequestStore :: Storer
pullrequestStore = numHelper "pullrequest" Github.pullRequest' $ \n ->
	store ("pullrequest" </> show n)

milestonesStore :: Storer
milestonesStore = simpleHelper "milestone" Github.Issues.Milestones.milestones' $
	forValues $ \req m -> do
		let n = Github.milestoneNumber m
		store ("milestone" </> show n) req m

issuesStore :: Storer
issuesStore = withHelper "issue" (\a u r y ->
	Github.issuesForRepo' a u r (y <> [Github.Open])
		>>= either (return . Left)
			(\xs -> Github.issuesForRepo' a u r
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
issuecommentsStore = numHelper "issuecomments" Github.Issues.Comments.comments' $ \n ->
	forValues $ \req c -> do
		let i = Github.issueCommentId c
		store ("issue" </> show n ++ "_comment" </> show i) req c

userrepoStore :: Storer
userrepoStore = simpleHelper "repo" Github.userRepo' $ \req r -> do
	store "repo" req r
	when (Github.repoHasWiki r == Just True) $
		updateWiki $ toGithubUserRepo r
	maybe noop addFork $ Github.repoParent r
	maybe noop addFork $ Github.repoSource r

forksStore :: Storer
forksStore = simpleHelper "forks" Github.forksFor' $ \req fs -> do
	storeSorted "forks" req fs
	mapM_ addFork fs

forValues :: (Request -> v -> Backup ()) -> Request -> [v] -> Backup ()
forValues a req vs = forM_ vs (a req)

type ApiCall v = Maybe Github.GithubAuth -> String -> String -> IO (Either Github.Error v)
type ApiWith v b = Maybe Github.GithubAuth -> String -> String -> b -> IO (Either Github.Error v)
type ApiNum v = ApiWith v Int
type Handler v = Request -> v -> Backup ()
type Helper = Request -> Backup ()

simpleHelper :: FilePath -> ApiCall v -> Handler v -> Helper
simpleHelper dest call handler req@(RequestSimple _ (GithubUserRepo user repo)) =
	deferOn dest req $ do
		auth <- getState gitHubAuth
		either (failedRequest req) (handler req) =<< liftIO (call auth user repo)
simpleHelper _ _ _ r = badRequest r

withHelper :: FilePath -> ApiWith v b -> b -> Handler v -> Helper
withHelper dest call b handler req@(RequestSimple _ (GithubUserRepo user repo)) =
	deferOn dest req $ do
		auth <- getState gitHubAuth
		either (failedRequest req) (handler req) =<< liftIO (call auth user repo b)
withHelper _ _ _ _ r = badRequest r

numHelper :: FilePath -> ApiNum v -> (Int -> Handler v) -> Helper
numHelper dest call handler req@(RequestNum _ (GithubUserRepo user repo) num) =
	deferOn dest req $ do
		auth <- getState gitHubAuth
		either (failedRequest req) (handler num req) =<< liftIO (call auth user repo num)
numHelper _ _ _ r = badRequest r

badRequest :: Request -> a
badRequest r = error $ "internal error: bad request type " ++ show r

{- When the specified file or directory already exists in git, the action
 - is deferred until later. -}
deferOn :: FilePath -> Request -> Backup () -> Backup ()
deferOn f req a = ifM (ingit $ storeLocation f req)
	( changeState $ \s -> s { deferredBackups = a : deferredBackups s }
	, a
	)
  where
	ingit f' = do
		h <- getCatFileHandle
		liftIO $ isJust <$> catObjectDetails h
			(Git.Types.Ref $ Git.Types.fromRef branchname ++ ":" ++ f')

getCatFileHandle :: Backup CatFileHandle
getCatFileHandle = go =<< getState catFileHandle
  where
  	go (Just h) = return h
	go Nothing = do
		h <- withIndex $ inRepo catFileStart
		changeState $ \s -> s { catFileHandle = Just h }
		return h

store :: Show a => FilePath -> Request -> a -> Backup ()
store filebase req val = do
	file <- (</>)
		<$> workDir
		<*> pure (storeLocation filebase req)
	liftIO $ do
		createDirectoryIfMissing True (takeDirectory file)
		writeFile file (ppShow val)

storeLocation :: FilePath -> Request -> FilePath
storeLocation filebase = location . requestRepo
  where
	location (GithubUserRepo user repo) =
		user ++ "_" ++ repo </> filebase

workDir :: Backup FilePath
workDir = (</>)
		<$> (Git.repoPath <$> getState gitRepo)
		<*> pure "github-backup.tmp"

storeSorted :: Ord a => Show a => FilePath -> Request -> [a] -> Backup ()
storeSorted file req val = store file req (sort val)

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
					[Param "reset", Param "-q", Param $ Git.Types.fromRef branchref, File "." ] r
				-- Stage workDir files into the index.
				h <- hashObjectStart r
				Git.UpdateIndex.streamUpdateIndex r
					[genstream dir h]
				hashObjectStop h
				-- Commit
				void $ Git.Branch.commit Git.Branch.AutomaticCommit False "github-backup" fullname [branchref] r
				removeDirectoryRecursive dir
  where
  	genstream dir h streamer = do
		fs <- filter (not . dirCruft) <$> dirContentsRecursive dir
		forM_ fs $ \f -> do
			sha <- hashFile h f
			path <- asTopFilePath <$> relPathDirToFile dir f
			streamer $ Git.UpdateIndex.updateIndexLine
				sha Git.Types.FileBlob path

{- Returns the ref of the github branch, creating it first if necessary. -}
getBranch :: Backup Git.Ref
getBranch = maybe (hasOrigin >>= create) return =<< branchsha
  where
  	create True = do
		inRepo $ Git.Command.run
			[Param "branch", Param $ Git.Types.fromRef branchname, Param $ Git.Types.fromRef originname]
		fromMaybe (error $ "failed to create " ++ Git.Types.fromRef branchname)
			<$> branchsha
	create False = withIndex $
		inRepo $ Git.Branch.commitAlways Git.Branch.AutomaticCommit "branch created" fullname []
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
fullname = Git.Ref $ "refs/heads/" ++ Git.Types.fromRef branchname

originname :: Git.Ref
originname = Git.Ref $ "refs/remotes/origin/" ++ Git.Types.fromRef branchname

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

addFork :: ToGithubUserRepo a => a -> Backup ()
addFork forksource = unlessM (elem fork . gitHubRemotes <$> getState gitRepo) $ do
	liftIO $ putStrLn $ "New fork: " ++ repoUrl fork
	void $ addRemote (remoteFor fork) (repoUrl fork)
	gitRepo' <- inRepo $ Git.Config.reRead
	changeState $ \s -> s { gitRepo = gitRepo' }

	gatherMetaData fork
  where
  	fork = toGithubUserRepo forksource
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

gatherMetaData :: GithubUserRepo -> Backup ()
gatherMetaData repo = do
	noforks <- getState noForks
	liftIO $ putStrLn $ "Gathering metadata for " ++ repoUrl repo ++ " ..."
	mapM_ call (filter (forksfilter noforks) toplevelApi)
  where
	call name = runRequest $ RequestSimple name repo
	forksfilter noforks name = not (noforks && name == "forks")

storeRetry :: [Request] -> Git.Repo -> IO ()
storeRetry [] r = void $ do
	try $ removeFile (retryFile r) :: IO (Either SomeException ()) 
storeRetry retryrequests r = writeFile (retryFile r) (show retryrequests)

loadRetry :: Git.Repo -> IO [Request]
loadRetry r = maybe [] (fromMaybe [] . readish)
	<$> catchMaybeIO (readFileStrict (retryFile r))

retryFile :: Git.Repo -> FilePath
retryFile r = Git.localGitDir r </> "github-backup.todo"

retry :: Backup ()
retry = do
	todo <- inRepo loadRetry
	unless (null todo) $ do
		liftIO $ putStrLn $
			"Retrying " ++ show (length todo) ++
			" requests that failed last time..."
		mapM_ runRequest todo
	changeState $ \s -> s
		{ retriedFailed = failedRequests s
		, failedRequests = S.empty
		, retriedRequests = S.fromList todo
		}

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
 -
 - Returns any requests that failed.
 -}
save :: Backup [Request]
save = do
	commitWorkDir
	failed <- getState failedRequests
	retriedfailed <- getState retriedFailed
	let toretry = S.toList failed ++ S.toList retriedfailed
	inRepo $ storeRetry toretry
	endState
	return toretry

showFailures :: [Request] -> IO ()
showFailures [] = noop
showFailures l = hPutStrLn stderr $ unlines $
	["Backup may be incomplete; " ++ 
		show (length l) ++ " requests failed:"
	] ++ map ("  " ++) (summarizeRequests l) ++ 
	[ "Run again later."
	]

newState :: Bool -> Git.Repo -> IO BackupState
newState noforks r = BackupState
	<$> pure S.empty
	<*> pure S.empty
	<*> pure S.empty
	<*> pure r
	<*> getAuth
	<*> pure []
	<*> pure Nothing
	<*> pure noforks

endState :: Backup ()
endState = liftIO . maybe noop catFileStop =<< getState catFileHandle

genBackupState :: Bool -> Git.Repo -> IO BackupState
genBackupState noforks repo = newState noforks =<< Git.Config.read repo

backupRepo :: Bool -> (Maybe Git.Repo) -> IO ()
backupRepo _ Nothing = error "not in a git repository, and nothing specified to back up"
backupRepo noforks (Just repo) =
	genBackupState noforks repo >>= evalStateT (runBackup go) >>= showFailures
  where
	go = do
		retry
		mainBackup
		runDeferred
		save

mainBackup :: Backup ()
mainBackup = do
	remotes <- gitHubPairs <$> getState gitRepo
	when (null remotes) $
		error "no github remotes found"
	forM_ remotes $ \(r, remote) -> do
		void $ fetchRepo r
		gatherMetaData remote

runDeferred :: Backup ()
runDeferred = go =<< getState deferredBackups
  where
	go [] = noop
	go l = do
		changeState $ \s -> s { deferredBackups = [] }
		void $ sequence l
		-- Running the deferred actions could cause
		-- more actions to be deferred; run them too.
		runDeferred

backupOwner :: Bool -> [GithubUserRepo] -> Owner -> IO ()
backupOwner noforks exclude (Owner name) = do
	auth <- getAuth
	l <- sequence
	 	[ Github.userRepos' auth name Github.All
		, Github.reposWatchedBy' auth name
		, Github.reposStarredBy auth name
		, Github.organizationRepos' auth name
		]
	let nameurls = nub $ mapMaybe makenameurl $ concat $ rights l
	when (null nameurls) $
		if (null $ rights l)
			then error $ unlines $ "Failed to query github for repos:" : map show (lefts l)
			else error $ "No GitHub repositories found for " ++ name
	-- Clone any missing repos, and get a BackupState for each repo
	-- that is to be backed up.
	states <- catMaybes <$> forM nameurls prepare
	-- First pass only retries things that failed before, so the
	-- retried actions will run in each repo before too much API is
	-- used up.
	states' <- forM states (execStateT . runBackup $ retry)
	states'' <- forM states' (execStateT . runBackup $ mainBackup)
	forM states'' (evalStateT . runBackup $ runDeferred >> save)
		>>= showFailures . concat
  where
	excludeurls = map repoUrl exclude
	
	makenameurl repo = 
#if MIN_VERSION_github(0,10,0)
		case Github.repoGitUrl repo of
			Just url -> Just (Github.repoName repo, url)
			Nothing -> Nothing
#else
		Just (Github.repoName repo, Github.repoGitUrl repo)
#endif

	prepare (dir, url)
		| url `elem` excludeurls = return Nothing
		| otherwise = do
			unlessM (doesDirectoryExist dir) $ do
				putStrLn $ "New repository: " ++ dir
				ok <- boolSystem "git"
					[ Param "clone"
					, Param url
					, Param dir
					]
				unless ok $ error "clone failed"
			Just <$> (genBackupState noforks =<< Git.Construct.fromPath dir)

data Options = Options
	{ includeOwner :: [Owner]
	, excludeRepo :: [GithubUserRepo]
	, noForksOpt :: Bool
	}
	deriving (Show)

data Owner = Owner String
	deriving (Show)

options :: Parser Options
options = Options <$> many owneropt <*> many excludeopt <*> noforksopt
  where
	owneropt = Owner <$> (argument str)
		( metavar "USERNAME|ORGANIZATION"
		<> help "Back up repositories owned by this entity."
		)
	excludeopt = parseUserRepo <$> (strOption
		( long "exclude"
		<> metavar "USERNAME/REPOSITORY"
		<> help "Skip backing up a repository."
		))
	noforksopt = switch
		( long "no-forks"
		<> help "Do not backup forks."
		)

parseUserRepo :: String -> GithubUserRepo
parseUserRepo s =
	let (user, repo) = separate (== '/') s
	in GithubUserRepo user repo

main :: IO ()
main = execParser opts >>= go
  where
	opts = info (helper <*> options)
		( fullDesc
		<> progDesc desc
		<> header "github-backup - backs up data from GitHub"
		)
	desc = unlines
		[ "Backs up all forks, issues, etc of a GitHub repository."
		, "Run without any parameters inside a clone of a repository to back it up."
		, "Or, specify whose repositories to back up."
		]
	go (Options owner exclude noforks)
		| null owner = backupRepo noforks =<< Git.Construct.fromCwd
		| otherwise = mapM_ (backupOwner noforks exclude) owner
