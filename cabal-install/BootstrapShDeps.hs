
{-

This program uses an already built cabal-install executable to create
the dependency list for bootstrap.sh.

The goals of the dependency generator are to create a reliable
bootstrap process, which can ignore extra packages installed in the
global and user package database. This is to help in more cases than
just bootstrapping on a freshly installed GHC.

If you have changed some of the packages which come with GHC in the
global package database then run bootstrap.sh, all bets are off,
otherwise the bootstrap.sh should always work with these dependencies.

Overview: run ghc-pkg list and get the unhidden global packages. Run
cabal install cabal-install --dry-run in a sandbox and get the extra
packages needed. This list is specific to the version of GHC you are
using.

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

4. run 'runhaskell BootstrapShDeps.hs > bootstrap-ghc-VER'

5. edit bootstrap.sh where it sets the DEPENDENCY_FILE variable

The same process can be used to update the dependencies for other
versions of GHC when Cabal/cabal-install is updated. (This should be
scripted since it is a little tedious.)

The format of the dependency files is:

line 1: the packages arguments to pass to ghc --make Setup.hs to
include all the needed packages which come with GHC
line 2: the same information, but as constraints to be passed to
./Setup

the rest of the lines contain the additional packages to download and
install in the correct order. These lines have the package name first,
then a space, then the version.

The reason the file is this weird format is to make it easy to use
from the bootstrap.sh without loads of crazy
regex/sed/awk/etc. hacking in the .sh file.

TODO: verbose mode - show commands being run, and output
better error handling to detect readProcess errors, etc.
add option to automatically delete the sandbox if it already exists
get all dependecies from cabal-install instead of cabal-install +
  ghc-pkg list
explicitly use the packages which come with ghc, and explicitly
  reinstall all the others always, even if they have also been added to
  the global package database
a script to generate all the deps files for supported versions of ghc
  for a new version of Cabal/cabal-install automatically.
a script to run automated tests for the bootstrap.sh on supported
  versions of ghc

expect binary tarballs in the tarballs dir
create a build dir
unpack each binary tarball in turn
set the path
generate the deps file or// run bootstrap and see if it succeeds


print:
ghc version to double check
first list ghc version dependencies
then list cabal install dependencies
e.g.

ghc-7.6.3
builtin-dependencies: array-0.5.0.0 etc. space separated
extra-dependencies: Cabal-1.22.0.0 etc. space separated

then use another helper for the bootstrap.sh:
split package into name and version? for the scripting

what does the script do?
check:
  temp dir
  CC
  LINK
  LD
  current dir
  ghc, ghc-pkg

set jobs arg

create sandbox

package args stuff

check installed packages
list of tasks:
download package if not there
unpack package fresh unconditionally
install package:
  build Setup
  configure
  build
  install





-}

import System.Process
import Data.List
import Data.Char
import Data.Tuple
import Control.Arrow
import System.Directory
import Control.Monad

main :: IO ()
main = do
    e <- doesDirectoryExist "bootstrap-deps-sandbox"
    when (e) $ void
      $ readProcess "cabal" ["sandbox", "delete"
                            ,"--sandbox=bootstrap-deps-sandbox"] ""
    _ <- readProcess "cabal" ["sandbox", "init"
                        ,"--sandbox=bootstrap-deps-sandbox"] ""
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
    -- output the packages which come with GHC in ghc argument format
    putStrLn $ "-hide-all-packages " ++ (intercalate " "
             $ map (\(a,b) -> ("-package " ++ a ++ "-" ++ b))
               keepGhc)
    -- output the packages which come with GHC in ./Setup argument format
    putStrLn $ intercalate " "
             $ map (\(a,b) -> ("--constraint=" ++ a ++ "==" ++ b))
               keepGhc
    -- output the other packages which must be installed before
    -- installing cabal-install
    putStrLn $ showPackageList noCabalInstall
  where
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
    -- remove stuff like (latest: 1.2.1.0) from end of lines
    noNotes x = if '(' `elem` x
                then reverse $ drop 1 $ dropWhile (/='(') $ reverse x
                else x
