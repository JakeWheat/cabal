
{-

Bootstrap cabal-install on a system without cabal-install.

It works by downloading and installing any needed dependencies for
cabal-install to a local sandbox. It then installs cabal-install
itself into this sandbox.

It expects to be run inside the cabal-install directory.

Usage:
runhaskell ./Bootstrap.hs


Known portability

tested on debian unstable in early 2015
tested with ghc 7.2.2, 7.4.2, 7.6.3, 7.8.4, 7.10.0-20141222

It should work on other posix systems ...

Doesn't currently use any shell, needs: ghc, c compiler (gcc, clang,
cc, icc found automatically), wget/curl/fetch, which, a tar that knows
how to run gzip, /dev/null.


Maintenance notes

The Bootstrap needs a dependency file which can be generated with this
program (or generated some other way or written by hand, etc.). The
dependency files for all supported versions of GHC are included with
the cabal-install source. To generate a dependency file:

you should have a copy of the git version of cabal-install. It expects
to find the correct version of Cabal.cabal at '../Cabal/Cabal.cabal'.

you should have a fresh install of the ghc version you want to generate
the dependencies for (at least, it should not have extra packages
installed to the global package database)

you need a working cabal-install binary

you should run 'cabal update' before generating the dependencies

Then run this:

runhaskell ./Bootstrap.hs deps

The dependencies for every supported version of GHC should be
regenerated for every release of cabal-install. You could also update
them every time you change the dependencies in the cabal-install.cabal
file.

Hint: to run Bootstrap on a development version of cabal-install, run
cabal sdist in the ../Cabal dir then move the
../Cabal/dist/Cabal-X.X.X.X.tar.gz tarball to the cabal-install
dir. (There is a small catch 22 here. This can be fixed by teaching
Bootstrap how to compile Cabal directly from the ../Cabal/ dir).

--------------------------

TODO ideas:

command line parsing
environment parsing?

?find and check linker
?check cc works
?use tmpdir same way as bootstrap.sh
?fix code to run gzip/tar like bootstrap.sh
install mode for git repo - find Cabal in ../Cabal instead of trying
  to download from hackage
timestamps on task output
[N/M] on task output for rough progress

test with bogus packages in global and user package databases

check downloaded package for integrity to give better error
message/help if download interrupted e.g.? Otherwise this is a
regression from bootstrap.sh which used to always redownload.

test with all ghc versions + generate deps
write script to regenerate deps for all ghc versions

generate dependencies: use cabal install to generate all dependencies
without ghc-pkg also.
generate dependencies: is there some way to ask ghc which packages
where installed with it and which have been added, then you don't need
a fresh ghc with untouched global package database to generate the
dependencies

test bootstrap on ci
test bootstrap on other operating systems on ci

-}

import System.Directory (doesFileExist,getCurrentDirectory
                        ,doesDirectoryExist,getDirectoryContents
                        ,getPermissions,executable,removeFile
                        ,setCurrentDirectory,removeDirectoryRecursive)
import System.FilePath ((</>), addExtension)
import System.IO (hClose,openFile, IOMode(..))
import Control.Monad (when,unless,msum,forM_)
import System.Exit (ExitCode(..),exitSuccess)
import System.Process (proc, std_in, std_out, std_err
                      ,StdStream(..), waitForProcess
                      ,readProcessWithExitCode,readProcess,createProcess)
import Data.List (isPrefixOf, partition, intercalate, isSuffixOf
                 ,isInfixOf, find)
import Data.Char (isSpace,isDigit)
import Control.Arrow ((***))
import Data.Tuple (swap)
import Control.Exception (bracket_)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Data.IORef

data Settings = Settings
    {hackageUrl :: String
    ,wget :: FilePath
    ,curl :: FilePath
    ,fetch :: FilePath
    ,tar :: FilePath
    ,scriptVerbose :: Bool
    ,dryRun :: Bool
    ,ghcCabalVerbose :: String
    ,commandLineDepsFile :: Maybe FilePath
    ,ghc :: FilePath
    ,ghcPkg :: FilePath
    ,cabalInstall :: FilePath
    ,ccOverride :: Maybe FilePath
    } deriving Show

defaultSettings :: Settings
defaultSettings = Settings
    {hackageUrl = "https://hackage.haskell.org/package"
                  --"file:///home/jake/wd/cabal/tarballs"
    ,wget = "wget"
    ,curl = "curl"
    ,fetch = "fetch"
    ,tar = "tar"
    ,scriptVerbose = True  -- set to True to output the main actions
                           -- this script does all the command lines
                           -- run and the output from most of the
                           -- commands run
    ,dryRun = False -- stop after reporting everything that will be
                    -- done without doing anything
    ,commandLineDepsFile = Nothing -- override this to use a different
                                   -- deps file
    ,ghcCabalVerbose = "" -- use "-v" e.g. for ghc Setup and ./Setup
                          -- to run with verbose flag you must also
                          -- set scriptVerbose to True to see the
                          -- output
    ,ghc = "ghc"
    ,ghcPkg = "ghc-pkg"
    ,cabalInstall = "cabal"
    ,ccOverride = Nothing
    }

main :: IO ()
main = do
    cwdOk <- doesFileExist $ "." </> "cabal-install.cabal"
    unless cwdOk $ error
        "The Bootstrap.hs program must be run in the cabal-install directory"
    args <- getArgs
    case args of
        ["deps"] -> generateDependencies defaultSettings
        [] -> bootstrapCabalInstall defaultSettings
        _ -> do
             putStrLn $ "Usage:\n" ++
                 "  'runhaskell ./Bootstrap.hs' to bootstrap cabal-install\n" ++
                 "  'runhaskell ./Bootstrap.hs deps' to generate dependency " ++
                 "file for current ghc"
             error $ "unrecognised args " ++ unwords args

bootstrapDepsFilename :: Settings -> String -> String
bootstrapDepsFilename s ghcVer =
    fromMaybe ("bootstrap-deps-ghc-" ++ ghcVer)
    $ commandLineDepsFile s

bootstrapCabalInstall :: Settings -> IO ()
bootstrapCabalInstall s = do
    let sv = scriptVerbose s
    when sv $ putStrLn "Checking what to do ...\n"
    -- ghc version stuff
    ghcVer <- strip <$> myReadProcess sv (ghc s) ["--numeric-version"]
    do
        -- check ghc and ghc-pkg have same version
        -- this check inherited from the bootstrap.sh script
        -- perhaps it should just rely on the path being set correctly?
        ghcPkgVer <- strip . reverse . takeWhile (/=' ') . reverse
                     <$> myReadProcess sv (ghcPkg s) ["--version"]
        when (ghcPkgVer /= ghcVer) $
            error $ "Version mismatch between " ++ ghc s
                       ++ "(" ++ ghcVer ++ ") and " ++ ghcPkg s
                       ++ "(" ++ ghcPkgVer ++ ")"
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
    -- find a cc
    cc <- findCC $ ccOverride s

    -- sandbox prep
    cabalInstallRoot <- getCurrentDirectory
    let sandboxPrefix = cabalInstallRoot </> ".cabal-sandbox"
    ghcArch <- getGhcArch
    let sandboxPkgDbPath =
            sandboxPrefix </> ghcArch ++  "-" ++ ghc s ++ "-" ++ ghcVer ++ "-packages.conf.d"
    sandboxPkgDbExists <- doesDirectoryExist sandboxPkgDbPath

    -- read dependency information
    -- todo: option to set on command line
    let dependencyFilename = bootstrapDepsFilename s ghcVer
    (builtinDependencies,extraDependenciesNeeded) <-
          parseDepsFile <$> readFile dependencyFilename
    -- deps already installed and deps to install
    (depsAlready,depsToInstall) <-
        if not sandboxPkgDbExists
        then return ([],extraDependenciesNeeded)
        else do
            sbPkgsRaw <- myReadProcess sv
                                       (ghcPkg s)
                                       ["list"
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
         else getDownloader sv
    -- create initial argument lists for ghc make Setup and ./Setup
    let ghcMakeSetupArgs =
            ["--make"
            ,"Setup"
            ,"-o"
            ,"Setup"
            ,"-" ++ ghcPkgArg
            ,sandboxPkgDbPath
            ,"-hide-all-packages"]
            ++  [ "-j" | ghcUseJobs]
            ++  [ ghcCabalVerbose s | not $ null $ ghcCabalVerbose s ]
    let cabalSetupConfigureArgs =
            ["--prefix"
            ,sandboxPrefix
            ,"--package-db=" ++ sandboxPkgDbPath
            ,"--disable-library-profiling"
            ,"--disable-shared"
            ,"--disable-split-objs"
            ,"--enable-executable-stripping"
            ,"--disable-tests"
            ,"--with-compiler=" ++ ghc s
            ,"--with-hc-pkg=" ++ ghcPkg s
            ,"--with-gcc=" ++ cc]
            ++  [ ghcCabalVerbose s | not $ null $ ghcCabalVerbose s ]

    -- summarize the tasks
    let unlessNull l x = if null l then "" else x
        concatNotNull f = let x = intercalate "\n" $ filter (not . null) f
                          in if null x then x else x ++ "\n"
    -- todo: add packages installed in global
    -- and packages installed in sandbox already??
    putStrLn $ "\nPlan\n\n"
        ++ concatNotNull
           [unlessNull tarballsToDownload
            $ "Download the following packages:\n"
               ++ intercalate "\n"
                  (map dropTarballExtension tarballsToDownload) ++ "\n"
           ,if sandboxPkgDbExists
            then ""
            else "Create package database at " ++ sandboxPkgDbPath ++ "\n"
           ,unlessNull depsToInstall
            $ "Install the following packages:\n"
              ++ intercalate "\n" depsToInstall ++ "\n"
           ,"Install cabal-install from local source"]
    -- todo: add program names to details
    when sv $ putStrLn $
        "Details\n\n"
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
            ++ hackageUrl s]
        ++ "ghc make Setup template:\n"
        ++ unwords (ghc s:ghcMakeSetupArgs) ++ "\n"
        ++ "Setup configure template:\n"
        ++ "./Setup configure " ++ unwords cabalSetupConfigureArgs ++ "\n"
        ++ "Settings: " ++ show s

    when (dryRun s) exitSuccess

    -----------------------------------
    trackInstalledPackages <- newIORef depsAlready

    -- helper functions for unpacking and installing packages
    let unpackPackage p = do
            deleteDirIfExists sv p
            runWithOutput sv (tar s) ["-xf", p `addExtension` "tar.gz"]
    let installPackage p = withDirectory sv p $ do
            setupExists <- doesFileExist "Setup"
            when setupExists $ do
                x <- isExecutable "./Setup"
                when x $ runWithOutput sv "./Setup" ["clean"]
                when sv $ putStrLn "rm Setup"
                removeFile "Setup"
            -- limit the packages available to 'ghc Setup.hs' and ./Setup
            -- a small trick used here is to keep the ghc Cabal if
            -- there is one until we install the new Cabal for this
            -- cabal-install. This doesn't seem to be needed for newer
            -- ghcs but when you specify two Cabal versions for old
            -- ghcs they fail
            let isCabal = ("Cabal-" `isPrefixOf`)
                hasCabal l = case find isCabal l of
                                Just _ -> True
                                Nothing -> False
                removeCabal = filter (not . isCabal)
                makeGhcPkgArgs = concatMap (\x -> ["-package",x])
                makeCabalPkgArgs = map (\x -> let (n,v) = splitPackageName x
                                              in "--constraint=" ++ n ++ "==" ++ v)
            ps <- do
                  is <- readIORef trackInstalledPackages
                  let builtins = if hasCabal is
                                 then removeCabal builtinDependencies
                                 else builtinDependencies
                  return $ builtins ++ is
            runWithOutput sv (ghc s) (ghcMakeSetupArgs ++ makeGhcPkgArgs ps)
            let verbose = [ ghcCabalVerbose s | not $ null $ ghcCabalVerbose s ]
                -- should use already installed cabal version to
                -- determine whether to use jobs but this should work
                -- effectively
                setupUseJobs = [ "-j" | ghcUseJobs ]
            runWithOutput sv
                "./Setup" $ "configure" : (verbose ++ cabalSetupConfigureArgs ++ makeCabalPkgArgs ps)
            runWithOutput sv
                "./Setup" $ "build" : (verbose ++ setupUseJobs)
            runWithOutput sv
                "./Setup" $ "install" : verbose
            modifyIORef trackInstalledPackages (p:)
    let installMessage p = do
            when sv $ putStrLn ""
            putStrLn $ "Installing " ++ p
            when sv $ putStrLn ""
    let doPackage p = do
            installMessage p
            unpackPackage p
            installPackage p

    ----------------------------------------------------------

    putStrLn "\nWorking\n"

    forM_ tarballsToDownload $ \tarball -> do
        let url = hackageUrl s ++ "/" ++ dropTarballExtension tarball ++ "/" ++ tarball
        putStrLn $ "Downloading " ++ url
        downloader url

    unless sandboxPkgDbExists $ do
        putStrLn $ "Create package database at " ++ sandboxPkgDbPath
        runWithOutput sv (ghcPkg s) ["init", sandboxPkgDbPath]

    mapM_ doPackage depsToInstall

    installMessage "cabal-install"
    installPackage "."

    ----------------------------------------
    -- finished, check the exe and let the user know
    let cabalDir = sandboxPrefix </> "bin"
        cabalPath = cabalDir </> "cabal"
    runWithOutput True cabalPath ["--version"]

    putStrLn $ "\n\ncabal-install has been installed at\n"
        ++ cabalPath ++ "\n"
        ++ "next steps:\n"
        ++ "  put the cabal binary in your path\n"
        ++ "  run 'cabal update'"
  where
    dropTarballExtension f =
        if ".tar.gz" `isSuffixOf` f
        then reverse $ drop 7 $ reverse f
        else error $ "couldn't drop tarball extension of name " ++ f
    getDownloader :: Bool -> IO (String, String -> IO ())
    getDownloader sv = do
         -- there is probably a better way to write this
         let findIt (p,d) = maybe Nothing (const $ Just (p,d)) <$> which p
         let errMsg = "Failed to find a downloader. " ++
                      "'curl', 'wget' or 'fetch' is required."
         fromMaybe (error errMsg) . msum <$> mapM findIt
             [(curl s, \url -> runWithOutput sv (curl s) ["-L","--fail","-O",url])
             ,(wget s, \url -> runWithOutput sv (wget s) ["-c", url])
             ,(fetch s, \url -> runWithOutput sv (fetch s) [url])]
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
    findCC (Just cc) = do
        whichCCm <- which cc
        maybe (error $ "couldn't find c compiler " ++ cc) return whichCCm
    findCC Nothing =
        let findCompiler [] = error "no compiler found (or could not be run)"
            findCompiler (c:cs) = do
                x <- which c
                maybe (findCompiler cs) return x
        in findCompiler ["gcc","clang", "cc", "icc"]
    getGhcArch = do
        -- we don't really need to do this, it just makes the prefix
        -- easy to convert to a real sandbox, which probably doesn't
        -- have any use
        ghcInfo <- myReadProcess (scriptVerbose s) (ghc s) ["--info"]
        let targetPlat = ("Target platform" `isInfixOf`)
        let targetLine = case filter targetPlat $ lines ghcInfo of
                             [x] -> x
                             [] -> error "Target platform field not found in output of ghc --info"
                             _ -> error "Multiple Target platform fields found in output of ghc --info"
        -- example
        -- ,("Target platform","x86_64-unknown-linux") -> x86_64-linux
        -- scan backwards till "
        -- scan until '-' this is the second part
        -- scan until '-' then until " this is the first part
        -- maybe my brain is backwards
        let a = reverse targetLine
            b = drop 1 $ dropWhile (/='"') a
            (os,c) = break (=='-') b
            d = drop 1 $ dropWhile (/='-') $ drop 1 c
            plat = takeWhile (/='"') d
        return (reverse os ++ "-" ++ reverse plat)
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

-}

generateDependencies :: Settings -> IO ()
generateDependencies s = do
    putStrLn "Please make sure you have run 'cabal update' recently"
    ghcVer <- strip <$> myReadProcess True (ghc s) ["--numeric-version"]
    let depsFile = bootstrapDepsFilename s ghcVer
    putStrLn $ "generating dependencies for ghc-" ++ ghcVer
               ++ " to file " ++ depsFile

    e <- doesDirectoryExist "bootstrap-deps-sandbox"
    -- TODO: just find a unused name for the sandbox
    when e $ error "please delete or rename the directory bootstrap-deps-sandbox"
    runWithOutput True (cabalInstall s) ["sandbox", "init"
                                        ,"--sandbox=bootstrap-deps-sandbox"]
    alwaysDeleteDir "bootstrap-deps-sandbox" $ do
    -- get the packages in the global package database
    -- assume these are only the packages which come with ghc
    ghcPkgList <- getGhcPackages
    -- get the list of extra dependencies which cabal-install says are
    -- needed to install a fresh cabal-install
    cabalInstallList <- getCabalInstallPackages
    -- filter out the packages which have old versions in the global
    -- package database which cabal-install wants to install newer
    -- versions of, so we only have one version for each package
    -- (apart from Cabal which is dealt with specially)
    let ghcSplit = map splitPackageName ghcPkgList
        cabalSplit = map splitPackageName cabalInstallList
        cabalNames = map fst cabalSplit
        keepGhcPkg ("Cabal",_) = True
        keepGhcPkg x = fst x`notElem` cabalNames
        keepGhc = filter keepGhcPkg ghcSplit
        noCabalInstall = filter ((/= "cabal-install") . fst) cabalSplit
    writeFile depsFile $
        "builtin-dependencies: "
        ++ unwords (map unsplitPackageName keepGhc)
        ++ "\n"
        ++ "extra-dependencies: "
        ++ unwords (map unsplitPackageName noCabalInstall)
        ++ "\n"
  where
    getGhcPackages =
        parseGhcPkgList <$> myReadProcess True (ghcPkg s) ["list", "--global"]
    getCabalInstallPackages = do
        cabalInstallList <- myReadProcess True
                                       (cabalInstall s)
                                       ["install"
                                       ,".." </> "Cabal"
                                       ,"."
                                       ,"--dry-run"]
        -- remove stuff like '(latest: 1.2.1.0)' from end of lines
        -- maybe there is an argument to cabal-install to not show these
        let noNotes x = if '(' `elem` x
                        then reverse $ drop 1 $ dropWhile (/='(') $ reverse x
                        else x
            cabalInstallListLine = "In order, the following would be installed"
            filterCabalInstallList [] =
                error $ "didn't find marker line in " ++ cabalInstall s
                        ++ " install cabal-install --dry-run"
            filterCabalInstallList (x:xs) | cabalInstallListLine `isPrefixOf` x = xs
            filterCabalInstallList (_:xs) = filterCabalInstallList xs
        return $ filterCabalInstallList
               $ filter (not . null)
               $ map noNotes
               $ lines cabalInstallList
    unsplitPackageName (a,b) = a ++ "-" ++ b
    alwaysDeleteDir d = bracket_ (return ()) (deleteDirIfExists True d)

-- split a package name+version into the name and the version
-- e.g. "Cabal-1.22.0.0" -> ("Cabal", "1.22.0.0")
splitPackageName :: String -> (String,String)
splitPackageName x = let y = reverse x
                         in ((reverse . tail) *** reverse)
                            $ swap $ break (=='-') y


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
myReadProcess :: Bool -> String -> [String] -> IO String
myReadProcess v s a = do
    when v $ putStrLn (unwords (s:filterArgs a))
    readProcess s (filterArgs a) ""
  where filterArgs = filter (not . null)

which :: FilePath -> IO (Maybe FilePath)
which f = do
    (c,p,_) <- readProcessWithExitCode "which" [f] ""
    case c of
        ExitSuccess -> return $ Just $ strip p
        _ -> return Nothing

strip :: String -> String
strip = let f = reverse . dropWhile isSpace in f . f

-- save the current directory, change to new directory, run the
-- function, then change back to the original directory automatically
withDirectory :: Bool -> FilePath -> IO a -> IO a
withDirectory v d f = do
    sv <- getCurrentDirectory
    when v $ putStrLn $ "cd " ++ d
    setCurrentDirectory d
    x <- f
    when v $ putStrLn $ "cd " ++ sv
    setCurrentDirectory sv
    return x

isExecutable :: FilePath -> IO Bool
isExecutable p = do
     per <- getPermissions p
     return $ executable per


deleteDirIfExists :: Bool -> FilePath -> IO ()
deleteDirIfExists v x = do
    y <- doesDirectoryExist x
    when y $ do
        when v $ putStrLn $ "rm -Rf " ++ x
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

runWithOutput :: Bool -> FilePath -> [String] -> IO ()
runWithOutput v cmd args = do
    when v $ putStrLn $ unwords (cmd:args)
    outErr <- if v
              then return Inherit
              else do
                   x <- openFile "/dev/null" ReadWriteMode
                   return $ UseHandle x
    (Just inh, _, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = outErr,
                                       std_err = outErr}
    -- not sure if this is the right way to do this
    hClose inh -- done with stdin
    -- wait on the process
    ex <- waitForProcess pid
    case outErr of
        UseHandle h -> hClose h
        _ -> return ()

    case ex of
     ExitSuccess   -> return ()
     ExitFailure r -> error $ "myRunProcess: " ++ cmd ++
                                     ' ':unwords (map show args) ++
                                     " (exit " ++ show r ++ ")"
