
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

import System.Directory (doesFileExist,getCurrentDirectory
                        ,doesDirectoryExist,getDirectoryContents
                        ,getPermissions,executable,removeFile
                        ,setCurrentDirectory,removeDirectoryRecursive)
import System.FilePath ((</>), addExtension)
import System.IO (hClose)
import Control.Monad (when,unless,msum,forM_)
import System.Exit (ExitCode(..))
import System.Process (proc, std_in, std_out, std_err
                      ,StdStream(..), waitForProcess
                      ,readProcessWithExitCode,readProcess,createProcess)
import Data.List (isPrefixOf, partition, intercalate, isSuffixOf)
import Data.Char (isSpace,isDigit)
import Control.Arrow ((***))
import Data.Tuple (swap)
import Control.Exception (bracket_)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

hackageUrl :: String
hackageUrl = "https://hackage.haskell.org/package"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["deps"] -> generateDependencies
        [] -> bootstrapCabalInstall
        _ -> do
             putStrLn $ "Usage:\n" ++
                 "  'runhaskell ./Bootstrap.hs' to bootstrap cabal-install\n" ++
                 "  'runhaskell ./Bootstrap.hs deps' to generate dependency " ++
                 "file for current ghc"
             error $ "unrecognised args " ++ unwords args

bootstrapCabalInstall :: IO ()
bootstrapCabalInstall = do
    cwdOk <- doesFileExist "./cabal-install.cabal"
    unless cwdOk $ error $
        "The Bootstrap.hs program must be run in the cabal-install directory"
    putStrLn "Checking what to do ...\n"
    let ghcCabalVerbose = ""
    -- ghc version stuff
    ghcVer <- strip <$> myReadProcess "ghc" ["--numeric-version"]
    let (ghcUseJobs,ghcPkgArg) =
          let (ghcMaj,ghcMin) = splitVer ghcVer
              -- is the ghc recent enough to support -j?
              js = case (ghcMaj,ghcMin) of
                           (Just x, Just y) -> (x,y) >= (7,8)
                           _ -> False
              -- what is the name of the argument to set the package
              -- database for this version of ghc?
              pkgarg = case splitVer ghcVer of
                             (Just x, Just y) | (x,y) <= (7,4) -> "package-conf"
                             _ -> "package-db"
          in (js, pkgarg)

    -- sandbox prep
    cabalInstallRoot <- getCurrentDirectory
    let sandboxPrefix = cabalInstallRoot </> ".cabal-sandbox"
        ghcArch = "x86_64-linux-ghc" -- TODO: do this properly
        sandboxPkgDbPath =
            sandboxPrefix </> ghcArch ++ "-" ++ ghcVer ++ "-packages.conf.d"
    sandboxPkgDbExists <- doesDirectoryExist sandboxPkgDbPath

    -- read dependency information
    -- todo: option to set on command line
    let dependencyFilename = "bootstrap-deps-ghc-" ++ ghcVer
    (builtinDependencies,extraDependenciesNeeded) <-
          parseDepsFile <$> readFile dependencyFilename
    -- deps already installed and deps to install
    (depsAlready,depsToInstall) <-
        if not sandboxPkgDbExists
        then return ([],extraDependenciesNeeded)
        else do
            sbPkgsRaw <- myReadProcess "ghc-pkg" ["list"
                                         ,"--" ++ ghcPkgArg
                                         ,sandboxPkgDbPath]
            let sbPkgsInstalled = parseGhcPkgList sbPkgsRaw
            return $ partition (`elem` sbPkgsInstalled) extraDependenciesNeeded
    -- tarballs present and tarballs to download
    (tarballsAlready,tarballsToDownload) <- do
         let wantedTarballs = map (`addExtension` "tar.gz") depsToInstall
         filesInCwd <- getDirectoryContents "."
         return $ partition (`elem` filesInCwd) wantedTarballs

    -- if there are tarballs to download, find a download program
    (downloadProg,downloader) <-
         if null tarballsToDownload
         then return ("",\_ -> error "no downloader requested")
         else getDownloader
    -- create initial argument lists for ghc make Setup and ./Setup
    let ghcMakeSetupArgs =
            ["--make"
            ,"Setup"
            ,"-o"
            ,"Setup"
            ,"-" ++ ghcPkgArg
            ,sandboxPkgDbPath]
            ++ (if ghcUseJobs then ["-j"] else [])
            ++ (if null ghcCabalVerbose then [] else [ghcCabalVerbose])
    let cabalSetupConfigureArgs =
            ["--prefix"
            ,sandboxPrefix
            ,"--package-db=" ++ sandboxPkgDbPath
            ,"--disable-library-profiling"
            ,"--disable-shared"
            ,"--disable-split-objs"
            ,"--enable-executable-stripping"
            ,"--disable-tests"]
            ++ (if null ghcCabalVerbose then [] else [ghcCabalVerbose])

    -- summarize the tasks
    let unlessNull l s = if null l then "" else s
        concatNotNull f = let x = intercalate "\n" $ filter (not . null) f
                          in if null x then x else x ++ "\n"
    putStrLn $ "\nPlan\n\n"
        ++ concatNotNull
           [unlessNull tarballsToDownload
            $ "Download the following packages:\n"
               ++ intercalate "\n"
                  (map dropTarballExtension tarballsToDownload) ++ "\n"
           ,unlessNull depsToInstall
            $ "Install the following dependencies:\n"
              ++ intercalate "\n" depsToInstall ++ "\n"
           ,"Install cabal-install from local source"]
        ++ "\n\nDetails\n\n"
        ++ "ghc version: " ++ ghcVer ++ "\n"
        ++ "Use jobs with ghc and cabal: " ++ show ghcUseJobs ++ "\n"
        ++ "Using dependencies from file: " ++ dependencyFilename ++ "\n"
        ++ "Use sandbox at " ++ sandboxPrefix ++ "\n"
        ++ "Use sandbox package database at " ++ sandboxPkgDbPath ++ "\n"
        ++ (if sandboxPkgDbExists
               then "Reusing existing sandbox package database\n"
               else "Creating new sandbox package database\n")
        ++ concatNotNull
           [unlessNull depsAlready $
            "Reusing already installed dependencies:\n"
            ++ intercalate "\n" depsAlready
           ,unlessNull tarballsAlready $
            "Using already downloaded tarballs:\n"
            ++ intercalate "\n" tarballsAlready
           ,unlessNull tarballsToDownload $
            "Using '" ++ downloadProg ++ "' to download tarballs from "
            ++ hackageUrl]
        ++ "ghc make Setup template:\n"
        ++ unwords ("ghc":ghcMakeSetupArgs) ++ "\n"
        ++ "Setup configure template:\n"
        ++ "./Setup configure " ++ unwords cabalSetupConfigureArgs ++ "\n"

    -----------------------------------

    putStrLn "\nWorking\n"

    unless sandboxPkgDbExists $
        runWithOutput "ghc-pkg" ["init", sandboxPkgDbPath]

    forM_ tarballsToDownload $ \tarball -> downloader $
        hackageUrl ++ "/" ++ dropTarballExtension tarball ++ "/" ++ tarball

    let unpackPackage p = do
            deleteDirIfExists p
            runWithOutput "tar" ["xf", p `addExtension` "tar.gz"]
    let installPackage p = withDirectory p $ do
            setupExists <- doesFileExist "Setup"
            when setupExists $ do
                per <- getPermissions "./Setup"
                when (executable per) $ runWithOutput "./Setup" ["clean"]
                putStrLn "rm Setup"
                removeFile "Setup"
            runWithOutput "ghc" ghcMakeSetupArgs
            let verbose = if null ghcCabalVerbose
                          then []
                          else [ghcCabalVerbose]
                -- should use already installed cabal version to
                -- determine whether to use jobs but this should work
                -- effectively
                setupUseJobs = if ghcUseJobs then ["-j"] else []
            runWithOutput "./Setup" $ ["configure"] ++ verbose
                                      ++ cabalSetupConfigureArgs
            runWithOutput "./Setup" $ ["build"] ++ verbose ++ setupUseJobs
            runWithOutput "./Setup" $ ["install"] ++ verbose
    let doPackage p = do
            putStrLn $ "\nInstalling " ++ p ++ "\n"
            unpackPackage p
            installPackage p

    mapM_ doPackage depsToInstall

    putStrLn "\nInstalling cabal-install\n"
    installPackage "."

    -- output final message to user
    let cabalDir = sandboxPrefix </> "bin"
        cabalPath = cabalDir </> "cabal"
    cabalBuilt <- doesFileExist cabalPath
    unless cabalBuilt $ error "cabal executable doesn't exist after building"
    cperms <- getPermissions cabalPath
    unless (executable cperms) $
        error "cabal executable isn't executable after buliding"
    putStrLn $ "\n\nThe 'cabal' program has been installed at\n"
        ++ cabalDir ++ "\n"
        ++ "Next steps:\n"
        ++ "  put the cabal binary in your path\n"
        ++ "  run 'cabal update'"
    return ()
  where
    dropTarballExtension f =
        if ".tar.gz" `isSuffixOf` f
        then reverse $ drop 7 $ reverse f
        else error $ "couldn't drop tarball extension of name " ++ f
    getDownloader = do
         -- there is probably a better way to write this
         let findIt (p,d) = do
                 (c,_,_) <- readProcessWithExitCode "which" [p] ""
                 case c of
                     ExitSuccess -> return $ Just (p,d)
                     _ -> return Nothing
         let errMsg = "Failed to find a downloader. " ++
                      "'curl', 'wget' or 'fetch' is required."
         fromMaybe (error errMsg) . msum <$> mapM findIt
             [("curl", \url -> runWithOutput "curl" ["-L","--fail","-O",url])
             ,("wget", \url -> runWithOutput "wget" ["-c", url])
             ,("fetch", \url -> runWithOutput "fetch" [url])]
    parseDepsFile f =
        let want x | null $ strip x = False
            want _ = True
            ls = filter want $ lines f
            f1 = "builtin-dependencies:"
            f2 = "extra-dependencies:"
        in case ls of
               [a,b] | f1 `isPrefixOf` a && f2 `isPrefixOf` b
                       -> (words $ drop (length f1) a
                          ,words $ drop (length f2) b)
                     | otherwise -> error $ "bad field names in " ++
                                    "dependency file, expected " ++ f1
                                    ++ " then " ++ f2 ++ ", got "
                                    ++ a ++ "\n" ++ b
               _ -> error "bad format in dependencies file, expected 2 lines"

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

get all dependencies from cabal-install instead of cabal-install +
  ghc-pkg list
a script to generate all the deps files for supported versions of ghc
  for a new version of Cabal/cabal-install automatically.
a script to run automated tests for the bootstrap.sh on supported
  versions of ghc

-}

generateDependencies :: IO ()
generateDependencies = do
    putStrLn "Please make sure you have run 'cabal update' recently"
    ghcVer <- strip <$> myReadProcess "ghc" ["--numeric-version"]
    let depsFile = "bootstrap-deps-ghc-" ++ ghcVer
    putStrLn $ "generating dependencies for ghc-" ++ ghcVer
               ++ " to file " ++ depsFile

    e <- doesDirectoryExist "bootstrap-deps-sandbox"
    -- TODO: just find a unused name for the sandbox
    when e $ error "please delete or rename the directory bootstrap-deps-sandbox"
    runWithOutput "cabal" ["sandbox", "init"
                          ,"--sandbox=bootstrap-deps-sandbox"]
    alwaysDeleteDir "bootstrap-deps-sandbox" $ do
    -- get the packages in the global package database
    -- assume these are only the packages which come with ghc
    ghcPkgList' <- myReadProcess "ghc-pkg" ["list", "--global"]
    let ghcPkgList = parseGhcPkgList ghcPkgList'
    -- get the list of extra dependencies which cabal-install says are
    -- needed to install a fresh cabal-install
    cabalInstallList' <- myReadProcess "cabal" ["install"
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
        -- hack: if Cabal isn't the first new package installed then
        -- also keep the Cabal from GHC in the package list
        -- hopefully this won't break anything ...
        nondupePackage x =
            let nm = fst x
            in case fst $ head cabalSplit of
                  "Cabal" -> nm `notElem` cabalNames
                  _ -> nm == "Cabal" || (nm `notElem` cabalNames)
        keepGhc = filter nondupePackage ghcSplit
        noCabalInstall = filter ((/= "cabal-install") . fst) cabalSplit
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
    -- remove stuff like '(latest: 1.2.1.0)' from end of lines
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

-- simple wrapper around readProcess
myReadProcess :: String -> [String] -> IO String
myReadProcess s a = do
    putStrLn (unwords (s:filterArgs a))
    readProcess s (filterArgs a) ""
  where filterArgs = filter (not . null)

strip :: String -> String
strip = let f = reverse . dropWhile isSpace in f . f

-- save the current directory, change to new directory, run the
-- function, then change back to the original directory automatically
withDirectory :: FilePath -> IO a -> IO a
withDirectory d f = do
    sv <- getCurrentDirectory
    putStrLn $ "cd " ++ d
    setCurrentDirectory d
    x <- f
    putStrLn $ "cd " ++ sv
    setCurrentDirectory sv
    return x

deleteDirIfExists :: FilePath -> IO ()
deleteDirIfExists x = do
    y <- doesDirectoryExist x
    when y $ do
        putStrLn $ "rm -Rf " ++ x
        removeDirectoryRecursive x

-- split a version number, just get the first two numbers
splitVer :: String -> (Maybe Integer,Maybe Integer)
splitVer x = let (a,x') = break (=='.') x
                 (b,_) = break (=='.') $ drop 1 x'
             in (safeRead a,safeRead b)
  where safeRead n = if all isDigit n
                     then Just $ read n
                     else Nothing


-- this runs a process without redirecting the standard out or error,
-- and without providing any standard input.

runWithOutput :: FilePath -> [String] -> IO ()
runWithOutput cmd args = do
    putStrLn $ unwords (cmd:args)
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
