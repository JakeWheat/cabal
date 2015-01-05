
{-

Bootstrap cabal-install on a system without cabal-install.

It works by downloading and installing any needed dependencies for
cabal-install to a local sandbox. It then installs cabal-install
itself into this sandbox.

It expects to be run inside the cabal-install directory.

Usage:
runhaskell ./Bootstrap.hs

Known portability: tested on debian unstable in early 2015
tested with ghc 7.2.2, 7.4.2, 7.6.3, 7.8.4, 7.10.0-20141222

It should work on other posix systems ...

The Bootstrap needs a dependency file which can be generated with this
program (or generated some other way or written by hand, etc.). Most
of the tine, Bootstrap will automatically use the correct dependency
file which was included in the cabal-install source.

Generating dependencies:

Usage:
runhaskell ./Bootstrap.hs deps

Prerequisites:

you should have a fresh install of the ghc version you want to
  generate the dependencies for (at least, it should not have extra
  packages installed to the global package database)
you need a working cabal-install binary
you should run 'cabal update' before generating the dependencies

You should regenerate the dependencies every release of cabal-install.

TODO:

write simple automated test to allow to refactor this code quickly
refactor, tidy up, etc.
sort out command line options and document
  custom deps file
check out the environment stuff and reproduce it here: CC, WGET, etc.
  find out about the temp dir stuff
  do the CC, LINK, etc. testing
  work around for no setEnv in earlier ghc
* limit the package access using -package, --constraint
* do the hack for Cabal and older versions of ghc
plan: always keep Cabal in the already installed list
drop Cabal from the constraints just before compiling Cabal, or if
  Cabal is already in the sandbox packagedb
get the sandbox pacakge db name properly
fix code to run gzip/tar like script ??
fix the post comments ??
test with bogus packages in global and user package databases
move all the options to the top

check downloaded package for integrity to give better error
  message/help if download interrupted e.g.

install mode for git repo - find Cabal
fix tar usage to be same as bootstrap.sh
--make: add package args, --with args, verbose

test with all ghc versions + generate deps
write script to generate deps?

command line options idea

./Bootstrap : bootstrap cabal
./Bootstrap deps : generate deps with current environment and write to appropriately named file
./Bootstrap deps --output f : write deps to file f
./Bootstrap --cc=gcc-4.4 override c compiler
same for --ghc, ghc_pkg, wget, curl, fetch, ter, gzip program?
tmpdir setting??
what about overriding these with env vars?
--dry-run ??
--verbose - to add verbose to ghc --make and ./Setup
../Bootstrap --use-deps filename


-}

import System.Directory (doesFileExist,getCurrentDirectory,doesDirectoryExist,getDirectoryContents
                        ,getPermissions,executable,removeFile
                        ,setCurrentDirectory,removeDirectoryRecursive)
import System.FilePath ((</>), addExtension)
import System.IO (hClose)
import Control.Monad (when,unless,void)
import System.Exit (exitFailure,ExitCode(..))
import System.Process (proc, std_in, std_out, std_err, StdStream(..), waitForProcess
                      ,readProcessWithExitCode,readProcess,createProcess)
import Data.List ((\\), intersect, isPrefixOf)
import Data.Char (isSpace,isDigit)
import Control.Arrow ((***))
import Data.Tuple (swap)
import Control.Exception (bracket, bracket_)
--import System.IO.Error
--import qualified Control.Exception as C
--import Control.Concurrent
import System.Environment (getArgs, getEnvironment)
import Control.Applicative ((<$>))
--import Control.Monad.Error
--import Debug.Trace
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["deps"] -> generateDependencies
        [] -> bootstrapCabalInstall
        _ -> do
             putStrLn $ "Usage:\n" ++
                     "  'runhaskell ./Bootstrap.hs' to bootstrap cabal-install\n" ++
                     "  'runhaskell ./Bootstrap.hs deps' to generate dependency file for current setup"
             error $ "unrecognised args " ++ show args

data Deps = Deps
            {builtinDependencies :: [String]
            ,extraDependencies :: [String]}
            deriving Show

bootstrapCabalInstall :: IO ()
bootstrapCabalInstall = do
    let verbose=""

    putStrLn "Checking which packages to download and which to install."
    ghcVer <- strip <$> runIt "ghc" ["--numeric-version"]
    -- todo: this doesn't work anymore
    args <- getArgs
    depsData <- do
         let depsFile = case args of
                        [] -> "bootstrap-deps-ghc-" ++ ghcVer
                        [a] -> a
                        _ -> error "please pass exactly one argument to set the dependency file manually, or no arguments to use a default dependency file"
         depsFileExist <- doesFileExist depsFile
         unless depsFileExist $ error $ "dependency file not found: " ++ depsFile
         f <- readFile depsFile
         return $ parseDepsFile f
    cwdOk <- doesFileExist "./cabal-install.cabal"
    unless cwdOk $ error "The Bootstrap.hs program must be run in the cabal-install directory"
    -- create sandbox
    currentDirectory <- getCurrentDirectory
    let sandboxPath = currentDirectory </> ".cabal-sandbox"
        ghcArch = "x86_64-linux-ghc" -- TODO: do this properly
        packageDb = sandboxPath </> ghcArch ++ "-" ++ ghcVer ++ "-packages.conf.d"
    packageDbAlready <- doesDirectoryExist packageDb
    unless packageDbAlready $ void $ runIt "ghc-pkg" ["init", packageDb]
    let ghcJobs = case splitVer ghcVer of
                         (Just x, Just y) | (x,y) >= (7,8) -> "-j"
                         _ -> ""
        ghcPkgArg = case splitVer ghcVer of
                         (Just x, Just y) | (x,y) <= (7,4) -> "package-conf"
                         _ -> "package-db"
    -- find which packages need to be installed
    alreadyInstalledPackagesRaw <- runIt "ghc-pkg" ["list"
                                                   ,"--" ++ ghcPkgArg
                                                   ,packageDb]
    let alreadyInstalledPackages = parseGhcPkgList alreadyInstalledPackagesRaw
        packagesToInstall = extraDependencies depsData \\ alreadyInstalledPackages
        packagesAlreadyInstalled = extraDependencies depsData `intersect` alreadyInstalledPackages
        wantedTarballs = map (\x -> (x,x `addExtension` "tar.gz")) packagesToInstall
    -- find which packages also need to be downloaded
    filesInCwd <- getDirectoryContents "."
    let tarballsToDownload = filter ((`notElem` filesInCwd) . snd) wantedTarballs
    -- give the user a summary of what will be done
    putStrLn ""
    unless (null packagesAlreadyInstalled) $
        putStrLn $ "The following packages are already installed and will be reused:\n"
                       ++ unlines packagesAlreadyInstalled
    unless (null tarballsToDownload) $
        putStrLn $ "The following packages will be downloaded:\n"
                           ++ unlines (map fst tarballsToDownload)
    putStrLn $ "The following packages will be installed:\n"
                       ++ unlines (packagesToInstall ++ ["cabal-install"])
    putStrLn ""

    -- download packages
    mapM_ (downloadPackage . fst) tarballsToDownload

    let unpackPackage p = do
            deleteDirIfExists p
            --putStrLn "tar"
            runIt2 "tar" ["xf", p `addExtension` "tar.gz"]
            -- TODO: check it looks like it worked?
    -- should check the existing Cabal version is 1.18 or greater
    -- in practice, I think checking that ghc supports jobs does essentially the same thing
    let cabalJobs = ghcJobs
        installPackage p = withDirectory p $ do
            setupExists <- doesFileExist "Setup"
            when setupExists $ do
                per <- getPermissions "./Setup"
                when (executable per) $ runIt2 "./Setup" ["clean"]
                putStrLn "rm Setup"
                removeFile "Setup"
            runIt2 "ghc"
                (words ("--make Setup -o Setup -" ++ ghcPkgArg)
                 ++ [packageDb, ghcJobs, verbose])
            let setupArgs = ["--prefix=" ++ sandboxPath
                            ,"--package-db=" ++ packageDb]
                       ++ words "--disable-library-profiling --disable-shared --disable-split-objs --enable-executable-stripping --disable-tests"
                       ++ [verbose]
            runIt2 "./Setup" ("configure":verbose:setupArgs)
            runIt2 "./Setup" ["build",verbose,cabalJobs]
            runIt2 "./Setup" ["install",verbose]
        doPackage p = do
            putStrLn $ "\nInstalling " ++ p ++ "\n"
            unpackPackage p
            installPackage p
    -- install the dependencies
    mapM_ doPackage packagesToInstall

    -- build cabal-install itself
    putStrLn "\nInstalling cabal-install\n"
    installPackage "."

    -- output final message to user
    let cabalDir = currentDirectory </> ".cabal-sandbox/bin/"
        cabalPath = cabalDir </> "cabal"
    cabalBuilt <- doesFileExist cabalPath
    let wentWrong = do
          putStrLn $ "Sorry, something went wrong.\n" ++
              "The 'cabal' executable was not successfully installed in\n" ++
              cabalDir
          exitFailure
    unless cabalBuilt wentWrong
    cperms <- getPermissions cabalPath
    unless (executable cperms) wentWrong
    myEnv <- getEnvironment
    let home = fromMaybe "$HOME" $ lookup "HOME" myEnv
    -- Question: is this text right?
    putStrLn $ "\n\nThe 'cabal' program has been installed at " ++ cabalDir ++ "\n"
          ++ "You should either add " ++ cabalDir ++ " to your PATH\n"
          ++ "or copy the cabal program to a directory that is on your PATH (recommended).\n"
          ++ "\n"
          ++ "The first thing to do is to get the latest list of packages with:\n"
          ++ "  cabal update\n"
          ++ "This will also create a default config file (if it does not already\n"
          ++ "exist) at " ++ home ++ "/.cabal/config\n"
          ++ "\n"
          ++ "By default cabal will install programs to " ++ home ++ "/.cabal/bin\n"
          ++ "If you do not want to add this directory to your PATH then you can\n"
          ++ "change the setting in the config file, for example you could use:\n"
          ++ "symlink-bindir: " ++ home ++ "/bin\n"
    return ()
  where
    downloadPackage p = do
        let hackageUrl = "https://hackage.haskell.org/package"
        let url = hackageUrl ++ "/" ++ p ++ "/" ++ p ++ ".tar.gz"
        d <- getDownloader
        d url
        let tarball = p `addExtension` "tar.gz"
        s <- doesFileExist tarball
        unless s $ error $ "Downloading " ++ url ++ " did not create " ++ tarball
        return ()
    getDownloader = do
        (c,_,_) <- readProcessWithExitCode "which" ["curl"] ""
        if c == ExitSuccess
          then return $ \url -> runIt2 "curl" ["-L","--fail","-O",url]
          else do
            (w,_,_) <- readProcessWithExitCode "which" ["wget"] ""
            if w == ExitSuccess
              then return $ \url -> runIt2 "wget" ["-c", url]
              else do
                (f,_,_) <- readProcessWithExitCode "which" ["fetch"] ""
                if f == ExitSuccess
                  then return $ \url -> runIt2 "fetch" [url]
                  else error "Failed to find a downloader. 'curl', 'wget' or 'fetch' is required."
    parseDepsFile f =
        let want x | null $ strip x = False
            want _ = True
            ls = filter want $ lines f
            f1 = "builtin-dependencies:"
            f2 = "extra-dependencies:"
        in case ls of
               [a,b] | f1 `isPrefixOf` a && f2 `isPrefixOf` b
                       -> Deps (words $ drop (length f1) a)
                               (words $ drop (length f2) b)
                     | otherwise -> error $ "bad field names in dependency file, expected " ++ f1 ++ " then " ++ f2 ++ ", got " ++ a ++ "\n" ++ b
               _ -> error "bad format in dependencies file, expected 2 lines"
--builtin-dependencies: array-0.5.0.0 etc. space separated
--extra-dependencies: Cabal-1.22.0.0 etc. space separated


{-

Generating the dependencies

Goal: work out exactly which packages and version numbers are needed,
both the packages which come with ghc, and the additional ones we need
to install if we have a new ghc install with no extra packages.

It works by listing all the non-hidden packages and versions which
come with ghc using ghc-pkg, and then all the extra packages and
versions that existing cabal-install binary needs to install the
current cabal-install source. You should run cabal update before
running this.

It would be better to ask cabal-install which versions of the packages
which come with ghc it needs, instead of using ghc-pkg, but there
doesn't seem to be a way to do that. Using ghc-pkg adds a few extra
packages which we don't need, but this should be harmless.

The reason to list all the packages is to use these dependencies to
improve the reliability of bootstrap running on a non pristine ghc
which is useful for debugging cabal-install, the bootstrap and for
recovering from deleting the cabal-install binary by accident.

TODO: verbose mode?
better error handling?

get all dependecies from cabal-install instead of cabal-install +
  ghc-pkg list
a script to generate all the deps files for supported versions of ghc
  for a new version of Cabal/cabal-install automatically.
a script to run automated tests for the bootstrap.sh on supported
  versions of ghc

-}

generateDependencies :: IO ()
generateDependencies = do
    putStrLn "Please make sure you have run 'cabal update' recently"
    ghcVer <- strip <$> runIt "ghc" ["--numeric-version"]
    let depsFile = "bootstrap-deps-ghc-" ++ ghcVer
    putStrLn $ "generating dependencies for ghc-" ++ ghcVer
               ++ " to file " ++ depsFile

    e <- doesDirectoryExist "bootstrap-deps-sandbox"
    when e $ error "please delete or rename the directory bootstrap-deps-sandbox"
    runIt2  "cabal" ["sandbox", "init"
                    ,"--sandbox=bootstrap-deps-sandbox"]
    alwaysDeleteDir "bootstrap-deps-sandbox" $ do
    -- get the packages in the global package database
    -- assume these are only the packages which come with ghc
    ghcPkgList' <- runIt "ghc-pkg" ["list", "--global"]
    let ghcPkgList = parseGhcPkgList ghcPkgList'
    {-let ghcPkgList = case lines ghcPkgList' of
                      [] -> error "ghc-pkg list --global returned no lines"
                      (_:xs) -> filter (not . null)
                                $ filter filterHidden $ map strip xs-}
    -- get the list of extra dependencies which cabal-install says are needed
    -- to install a fresh cabal-install
    cabalInstallList' <- runIt "cabal" ["install"
                                       ,"../Cabal"
                                       ,"."
                                       ,"--dry-run"]
    let cabalInstallList = filterCabalInstallList
                           $ filter (not . null)
                           $ map noNotes
                           $ lines cabalInstallList'
    -- filter out the packages which have old versions in the global
    -- package database which cabal-install wants to install newer
    -- versions of, so we only have one version for each package
    let ghcSplit = map splitPackageName ghcPkgList
        cabalSplit = map splitPackageName cabalInstallList
        cabalNames = map fst cabalSplit
        -- hack: if Cabal isn't the first new package installed
        -- then also keep the Cabal from GHC in the package list
        -- hopefully this won't break anything ...
        nondupePackage x =
            let nm = fst x
            in case fst $ head cabalSplit of
                  "Cabal" -> nm `notElem` cabalNames
                  _ -> nm == "Cabal" || (nm `notElem` cabalNames)
        keepGhc = filter nondupePackage ghcSplit
        noCabalInstall = filter ((/= "cabal-install") . fst) cabalSplit
        --showPackageList = intercalate "\n" . map (\(a,b) -> a ++ " " ++ b)
        --unsplitPackageName (a,b) = a ++ "-" ++ b
    writeFile depsFile $
        "builtin-dependencies: "
        ++ unwords (map unsplitPackageName keepGhc)
        ++ "\n"
        ++ "extra-dependencies: "
        ++ unwords (map unsplitPackageName noCabalInstall)
        ++ "\n"
  where
    alwaysDeleteDir d = bracket_ (return ()) (deleteDirIfExists d)
    cabalInstallListLine = "In order, the following would be installed"
    filterCabalInstallList [] =
        error "didn't find marker line in cabal install cabal-install --dry-run"
    filterCabalInstallList (x:xs) | cabalInstallListLine `isPrefixOf` x = xs
    filterCabalInstallList (_:xs) = filterCabalInstallList xs
    -- split a package name+version into the name and the version
    -- e.g. "Cabal-1.22.0.0" -> ("Cabal", "1.22.0.0")
    splitPackageName x = let y = reverse x
                         in ((reverse . tail) *** reverse)
                            $ swap $ break (=='-') y
    unsplitPackageName (a,b) = a ++ "-" ++ b
    -- remove stuff like (latest: 1.2.1.0) from end of lines
    -- maybe there is an argument to cabal-install to not show these
    noNotes x = if '(' `elem` x
                then reverse $ drop 1 $ dropWhile (/='(') $ reverse x
                else x

parseGhcPkgList :: String -> [String]
parseGhcPkgList s =
        let ls = map strip $ lines s
            keepIt x = case x of
              "" -> False
              '/':_ -> False
              '(':_ -> False
              _ -> True
        in filter keepIt ls


runIt2 :: String -> [String] -> IO ()
runIt2 s a = putStrLn (unwords (s:filterArgs a))
                 >> myRunProcess s (filterArgs a)
  where filterArgs = filter (not . null)

runIt :: String -> [String] -> IO String
runIt s a = putStrLn (unwords (s:filterArgs a))
            >> readProcess s (filterArgs a) ""
  where filterArgs = filter (not . null)

strip :: String -> String
strip = let f = reverse . dropWhile isSpace in f . f

withDirectory :: String -> IO a -> IO a
withDirectory d f =
        bracket (do
                 sv <- getCurrentDirectory
                 putStrLn $ "cd " ++ d
                 setCurrentDirectory d
                 return sv)
                (\od -> do
                 putStrLn $ "cd " ++ od
                 setCurrentDirectory od)
                $ const f

deleteDirIfExists :: FilePath -> IO ()
deleteDirIfExists x = do
        y <- doesDirectoryExist x
        when y $ do
                 putStrLn $ "rm -Rf " ++ x
                 removeDirectoryRecursive x

splitVer :: String -> (Maybe Integer,Maybe Integer)
splitVer x = let (a,x') = break (=='.') x
                 (b,_) = break (=='.') $ drop 1 x'
             in (safeRead a,safeRead b)

safeRead :: String -> Maybe Integer
safeRead n = if all isDigit n
             then Just $ read n
             else Nothing


-- this runs a process without redirecting the standard out or error,
-- and without having any standard input. It throws an io error if the
-- process returns a non-zero exit code.

myRunProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ arguments
    -> IO ()
myRunProcess cmd args = do
    (Just inh, _, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = Inherit,
                                       std_err = Inherit}
    -- not sure if this is the right way to do this
    hClose inh -- done with stdin
    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return ()
     ExitFailure r -> error $ "myRunProcess: " ++ cmd ++
                                     ' ':unwords (map show args) ++
                                     " (exit " ++ show r ++ ")"
