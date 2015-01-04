{-

bootstrap cabal-install on a system without cabal-install.

This needs a dependency file, see GenerateBootstrapDeps.hs

TODO:
limit the package access
do the hack for Cabal and older versions of ghc
add verbose modes and hide stuff otherwise
review using env vars
find out about the temp dir stuff
check the CC, LINK, etc.
check using custom deps file
get the sandbox package name properly

do dry run mode

add the post comments

test with bogus packages in global and user package databases
download packages

do jobs properly

install mode for git repo - find Cabal
fix tar usage to be same as bootstrap.sh
--make: add package args, --with args, verbose

test with all ghc versions + generate deps
write script to generate deps?

hlint, tidy, document, etc.

-}

import System.Directory
import System.FilePath
import System.IO
import Control.Monad
import System.Exit
import System.Process
import Data.List
import Data.Char
import Control.Arrow
import Data.Tuple
import Control.Exception
import System.IO.Error
import qualified Control.Exception as C
import Control.Concurrent
import System.Environment
import Control.Applicative
--import Control.Monad.Error

die :: String -> IO ()
die e = hPutStrLn stderr e >> exitWith (ExitFailure 1)

data Deps = Deps
            {builtinDependencies :: [String]
            ,extraDependencies :: [String]}
            deriving Show

main :: IO ()
main = do
    -- check for verbose
    -- check CC, LINK, LD, GHC, GHC-PKG
    ghcVer <- strip <$> runIt "ghc" ["--numeric-version"]
    -- check running in right directory
    -- lazily find a download program
    x <- getArgs
    depsData <- do
         let depsFile = case x of
                        [] -> "bootstrap-deps-ghc-" ++ ghcVer
                        [a] -> a
                        _ -> error $ "please pass exactly one argument to set the dependency file manually, or no arguments to use a default dependency file"
         putStrLn $ "[" ++ depsFile ++ "]"
         depsFileExist <- doesFileExist depsFile
         unless depsFileExist $ error $ "dependency file not found: " ++ depsFile
         f <- readFile depsFile
         putStrLn $ "----\n" ++ f ++ "\n----"
         return $ parseDepsFile f
    putStrLn $ show depsData
    cwdOk <- doesFileExist "./cabal-install.cabal"
    unless cwdOk $ die "The Bootstrap.hs program must be run in the cabal-install directory"
    -- create sandbox
    currentDirectory <- getCurrentDirectory
    let sandboxPath = currentDirectory </> ".cabal-sandbox"
        ghcArch = "x86_64-linux-ghc" -- TODO: do this properly
        packageDb = sandboxPath </> ghcArch ++ "-" ++ ghcVer ++ "-packages.conf.d"
    putStrLn packageDb
    packageDbAlready <- doesDirectoryExist packageDb
    putStrLn $ show packageDbAlready
    unless packageDbAlready $ void $ runIt "ghc-pkg" ["init", packageDb]
    let (a,b) = splitVer ghcVer
        ghcJobs = case (safeRead a, safeRead b) of
                         (Just x, Just y) | x >= 7 && y >= 9 -> "-j"
                         _ -> ""
        ghcPkgArg = case (safeRead a, safeRead b) of
                         (Just x, Just y) | x <= 7 && y <= 4 -> "package-conf"
                         _ -> "package-db"
    -- find which packages need to be installed
    alreadyInstalledPackagesRaw <- runIt "ghc-pkg" ["list"
                                                   ,"--" ++ ghcPkgArg
                                                   ,packageDb]
    putStrLn alreadyInstalledPackagesRaw
    let alreadyInstalledPackages = parseGhcPkgList alreadyInstalledPackagesRaw
        packagesToInstall = extraDependencies depsData \\ alreadyInstalledPackages
        wantedTarballs = map (`addExtension` "tar.gz") packagesToInstall
    -- find which packages also need to be downloaded
    filesInCwd <- getDirectoryContents "."
    let tarballsToDownload = wantedTarballs \\ filesInCwd
    putStrLn $ show tarballsToDownload

    -- TODO: consider installing Cabal from relative path if not found
    -- and we are in a git repo. This is to help running bootstrap for a
    -- unrelease cabal-install

    -- download package: TODO
    -- unpack and install package
    let unpackPackage p = do
            putStrLn $ "unpack " ++ p
            deleteDirIfExists p
            putStrLn "tar"
            runIt2 "tar" ["xf", p ++ ".tar.gz"]
        cabalJobs = ""
        --(cabalMaj,cabalMin) = splitVer
    let installPackage p = withDirectory p $ do
            putStrLn $ "install " ++ p
            -- let (packageName,packageVersion) = splitPackageName p
            setupExists <- doesFileExist "Setup"
            when setupExists $ do
                -- todo: check if executable
                runIt2 "./Setup" ["clean"]
                removeFile "Setup"
            runIt2 "ghc"
                (words ("--make Setup -o Setup -" ++ ghcPkgArg)
                 ++ [packageDb, ghcJobs])
                -- ${GHC} --make Setup -o Setup ${GHC_PACKAGE_ARGS} ${GHC_JOBS} ${GHC_CONSTRAINTS}
            let args = ["--prefix=" ++ sandboxPath
                       ,"--package-db=" ++ packageDb]
                       ++ words "--disable-library-profiling --disable-shared --disable-split-objs --enable-executable-stripping --disable-tests"
    -- args="${CABAL_PACKAGE_ARGS} --prefix=${SANDBOX} --with-compiler=${GHC}"
    -- args="$args --with-hc-pkg=${GHC_PKG} --with-gcc=${CC} --with-ld=${LD}"
    -- args="$args --disable-library-profiling --disable-shared"
    -- args="$args --disable-split-objs --enable-executable-stripping"
    -- args="$args --disable-tests ${VERBOSE} $CONSTRAINTS"
            runIt2 "./Setup" ("configure":args)
            runIt2 "./Setup" ["build", cabalJobs]
            runIt2 "./Setup" ["install"]
        doPackage p = unpackPackage p >> installPackage p
    mapM_ doPackage packagesToInstall
    installPackage "."

    -- build cabal-install itself

    return ()
  where
    withDirectory d f =
        bracket (do
                 sv <- getCurrentDirectory
                 setCurrentDirectory d
                 return sv)
                (setCurrentDirectory)
                $ const f
    runIt s a = putStrLn (intercalate " " (s:a))
                >> readProcess s a ""
    runIt2 s a = putStrLn (intercalate " " (s:a))
                 >> myReadProcess s a ""
    parseGhcPkgList s =
        let ls = map strip $ lines s
            keepIt x = case x of
              "" -> False
              '/':_ -> False
              '(':_ -> False
              _ -> True
        in filter keepIt ls
    strip = let f = reverse . dropWhile isSpace in f . f
    -- split a package name+version into the name and the version
    -- e.g. "Cabal-1.22.0.0" -> ("Cabal", "1.22.0.0")
    splitPackageName x = let y = reverse x
                         in ((reverse . tail) *** reverse)
                            $ swap $ break (=='-') y
    deleteDirIfExists x = do
        y <- doesDirectoryExist x
        when y $ removeDirectoryRecursive x
    splitVer x = let (a,x') = break (=='.') x
                     (b,x'') = break (=='.') $ drop 1 x'
                 in (a,b)
    safeRead :: String -> Maybe Integer
    safeRead n = if all isDigit n
                 then Just $ read n
                 else Nothing
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
               _ -> error $ "bad format in dependencies file, expected 2 lines"
--builtin-dependencies: array-0.5.0.0 etc. space separated
--extra-dependencies: Cabal-1.22.0.0 etc. space separated


myReadProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO ()
myReadProcess cmd args input = do
    (Just inh, _, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = Inherit,
                                       std_err = Inherit }

    -- fork off a thread to start consuming the output
    --output  <- hGetContents outh
    --outMVar <- newEmptyMVar
    --forkIO $ C.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    --takeMVar outMVar
    --hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return ()
     ExitFailure r -> error $ "myReadProcess: " ++ cmd ++
                                     ' ':unwords (map show args) ++
                                     " (exit " ++ show r ++ ")"
      {-
      ioError (mkIOError OtherError ("readProcess: " ++ cmd ++ 
                                     ' ':unwords (map show args) ++ 
                                     " (exit " ++ show r ++ ")")
                                 Nothing Nothing) -}
