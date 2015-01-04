{-

bootstrap cabal-install on a system without cabal-install.

This needs a dependency file, see GenerateBootstrapDependencies.hs

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
--import Control.Monad.Error

die :: String -> IO ()
die e = hPutStrLn stderr e >> exitWith (ExitFailure 1)

tempWantPackages :: [String]
tempWantPackages =
    ["Cabal-1.22.0.0"
    ,"network-2.6.0.2"
    ,"random-1.1"
    ,"stm-2.4.4"
    ,"text-1.2.0.3"
    ,"transformers-0.4.2.0"
    ,"mtl-2.2.1"
    ,"parsec-3.1.7"
    ,"network-uri-2.6.0.1"
    ,"HTTP-4000.2.19"
    ,"zlib-0.5.4.2"]


main :: IO ()
main = do
  -- check for verbose
  -- check CC, LINK, LD, GHC, GHC-PKG
  -- check running in right directory
  -- lazily find a download program
  cwdOk <- doesFileExist "./cabal-install.cabal"
  unless cwdOk $ die "The Bootstrap.hs program must be run in the cabal-install directory"
  -- create sandbox
  currentDirectory <- getCurrentDirectory
  let sandboxPath = currentDirectory </> ".cabal-sandbox"
      packageDb = sandboxPath </> "x86_64-linux-ghc-7.8.4-packages.conf.d"
  putStrLn packageDb
  packageDbAlready <- doesDirectoryExist packageDb
  putStrLn $ show packageDbAlready
  unless packageDbAlready $ void $ runIt "ghc-pkg" ["init", packageDb]
  -- find which packages need to be installed
  alreadyInstalledPackagesRaw <- runIt "ghc-pkg" ["list"
                                                 ,"--package-db"
                                                 ,packageDb]
  putStrLn alreadyInstalledPackagesRaw
  let alreadyInstalledPackages = parseGhcPkgList alreadyInstalledPackagesRaw
      packagesToInstall = tempWantPackages \\ alreadyInstalledPackages
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
          deleteDirIfExists p
          runIt2 "tar" ["xf", p ++ ".tar.gz"]
  let installPackage p = withDirectory p $ do
          -- let (packageName,packageVersion) = splitPackageName p
          putStrLn =<< runIt "ghc"
              (words "--make Setup -o Setup -package-db "
               ++ [packageDb, "-j"])
          let args = ["--prefix=" ++ sandboxPath
                     ,"--package-db=" ++ packageDb]
                     ++ words "--disable-library-profiling --disable-shared --disable-split-objs --enable-executable-stripping --disable-tests"
  -- args="${CABAL_PACKAGE_ARGS} --prefix=${SANDBOX} --with-compiler=${GHC}"
  -- args="$args --with-hc-pkg=${GHC_PKG} --with-gcc=${CC} --with-ld=${LD}"
  -- args="$args --disable-library-profiling --disable-shared"
  -- args="$args --disable-split-objs --enable-executable-stripping"
  -- args="$args --disable-tests ${VERBOSE} $CONSTRAINTS"
          runIt2 "./Setup" ("configure":args)
          runIt2 "./Setup" ["build","-j"]
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
    deleteDirIfExists x =
        removeDirectoryRecursive x


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
