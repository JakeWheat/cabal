
{-

This program uses an already built cabal-install executable to create
the dependency list for bootstrap.sh.

It generates a list of package which come with GHC, and an additional
list of packages to add to this so that cabal-install can be built.

These lists are used control exactly what packages are used when
bootstrap.hs builds the dependencies and cabal-install. This is to
help increase the reliability when you have extra packages installed
in your global and/or user package database.

Operation overview: run ghc-pkg list and get the unhidden global
packages. Run cabal install cabal-install --dry-run in a sandbox and
get the extra packages needed. This list is specific to the version of
GHC you are using.

Using ghc-pkg list along with cabal-install is a replacement for
asking cabal-install for all the dependencies and versions, since
there doesn't appear to be a way to run cabal install cabal-install
--dry-run --show-versions-of-installed-packages or something which
shows which versions of already installed packages it would use. This
could probably be implemented without too much trouble.

Generating the dependencies like this avoids trying to write a poor
man's constraint solver in the bootstrap.sh.

How to create a new set of dependencies for a new version of GHC:

1. install GHC from source or binary with default packages (not
haskell platform for instance)

2. make sure you have a working cabal-install binary. One way of doing
this on aa new platform is to bootstrap on a older version of GHC or
cabal-install which is already working.

3. run 'cabal update' to get the latest package descriptions from
hackage

4. run 'runhaskell GenerateBootstrapDeps.hs > bootstrap-ghc-VER'

The same process can be used to update the dependencies for other
versions of GHC when Cabal/cabal-install is updated. (This should be
scripted since it is a little tedious.)

The format of the dependency files is:
line 1:
builtin-dependencies: package-ver package-ver

this has all the dependencies which we expect to find in the global
package database

line 2:
extra-dependencies: package-ver package-ver
all the extra dependencies to install to allow cabal-install to build

example (for unreleased cabal-install 1.22.0.0):

builtin-dependencies: array-0.5.0.0 base-4.7.0.2 bin-package-db-0.0.0.0 binary-0.7.1.0 rts-1.0 bytestring-0.10.4.0 containers-0.5.5.1 deepseq-1.3.0.2 directory-1.2.1.0 filepath-1.3.0.2 ghc-prim-0.3.1.0 haskeline-0.7.1.2 hoopl-3.10.0.1 hpc-0.6.0.1 integer-gmp-0.5.1.0 old-locale-1.0.0.6 old-time-1.1.0.2 pretty-1.1.1.1 process-1.2.0.0 template-haskell-2.9.0.0 terminfo-0.4.0.0 time-1.4.2 unix-2.7.0.1 xhtml-3000.2.1
extra-dependencies: Cabal-1.22.0.0 network-2.6.0.2 random-1.1 stm-2.4.4 text-1.2.0.3 transformers-0.4.2.0 mtl-2.2.1 parsec-3.1.7 network-uri-2.6.0.1 HTTP-4000.2.19 zlib-0.5.4.2

TODO: verbose mode - show commands being run, and output
better error handling?
always delete the sandbox before exiting, and error if it already
  exists
get all dependecies from cabal-install instead of cabal-install +
  ghc-pkg list
a script to generate all the deps files for supported versions of ghc
  for a new version of Cabal/cabal-install automatically.
a script to run automated tests for the bootstrap.sh on supported
  versions of ghc

expect binary tarballs in the tarballs dir
create a build dir
unpack each binary tarball in turn
set the path
generate the deps file or// run bootstrap and see if it succeeds



-}

import System.Process
import Data.List
import Data.Char
import Data.Tuple
import Control.Arrow
import System.Directory
import Control.Monad
import Control.Exception

main :: IO ()
main = do
    e <- doesDirectoryExist "bootstrap-deps-sandbox"
    when (e) $ error "please delete or rename the directory bootstrap-deps-sandbox"
    _ <- readProcess "cabal" ["sandbox", "init"
                        ,"--sandbox=bootstrap-deps-sandbox"] ""
    alwaysDeleteDir "bootstrap-deps-sandbox" $ do
    -- get the packages in the global package database
    -- assume these are only the packages which come with ghc
    ghcPkgList' <- readProcess "ghc-pkg" ["list", "--global"] ""
    let ghcPkgList = case lines ghcPkgList' of
                      [] -> error "ghc-pkg list --global returned no lines"
                      (_:xs) -> filter (not . null)
                                $ filter filterHidden $ map strip xs
    -- get the list of extra dependencies which cabal-install says are needed
    -- to install a fresh cabal-install
    cabalInstallList' <- readProcess "cabal" ["install"
                                             ,"../Cabal"
                                             ,"."
                                             ,"--dry-run"] ""
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
            in case fst $ head $ cabalSplit of
                  "Cabal" -> nm `notElem` cabalNames
                  _ -> if nm == "Cabal"
                       then True
                       else nm `notElem` cabalNames
        keepGhc = filter nondupePackage ghcSplit
        noCabalInstall = filter ((/= "cabal-install") . fst) cabalSplit
        showPackageList = intercalate "\n" . map (\(a,b) -> a ++ " " ++ b)
        unsplitPackageName (a,b) = a ++ "-" ++ b
    putStrLn $ "builtin-dependencies: "
               ++ intercalate " " (map unsplitPackageName keepGhc)
    putStrLn $ "extra-dependencies: "
               ++ intercalate " " (map unsplitPackageName noCabalInstall)
    -- output the packages which come with GHC in ghc argument format
    --putStrLn $ "-hide-all-packages " ++ (intercalate " "
    --         $ map (\(a,b) -> ("-package " ++ a ++ "-" ++ b))
    --           keepGhc)
    -- output the packages which come with GHC in ./Setup argument format
    --putStrLn $ intercalate " "
    --         $ map (\(a,b) -> ("--constraint=" ++ a ++ "==" ++ b))
    --           keepGhc
    -- output the other packages which must be installed before
    -- installing cabal-install
    --putStrLn $ showPackageList noCabalInstall
  where
    alwaysDeleteDir d f =
        bracket (return ())
                (const $ deleteDirIfExists d)
                $ const f
    deleteDirIfExists x = do
        y <- doesDirectoryExist x
        when y $ removeDirectoryRecursive x
    cabalInstallListLine = "In order, the following would be installed"
    filterCabalInstallList [] =
        error "didn't find marker line in cabal install cabal-install --dry-run"
    filterCabalInstallList (x:xs) | cabalInstallListLine `isPrefixOf` x = xs
    filterCabalInstallList (_:xs) = filterCabalInstallList xs
    strip = let f = dropWhile isSpace . reverse
            in f . f
    filterHidden ('(':_) = False
    filterHidden _ = True
    -- split a package name+version into the name and the version
    -- e.g. "Cabal-1.22.0.0" -> ("Cabal", "1.22.0.0")
    splitPackageName x = let y = reverse x
                         in ((reverse . tail) *** reverse)
                            $ swap $ break (=='-') y
    unsplitPackageName (a,b) = a ++ "-" ++ b
    -- remove stuff like (latest: 1.2.1.0) from end of lines
    noNotes x = if '(' `elem` x
                then reverse $ drop 1 $ dropWhile (/='(') $ reverse x
                else x
