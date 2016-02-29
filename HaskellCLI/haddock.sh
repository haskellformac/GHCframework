#!/bin/sh

appContainer="$HOME/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support"
libGHC="/usr/local/lib/ghc-VERSION"
exedir="$appContainer/lib/ghc/bin"

exeprog="haddock"
topdir="$appContainer/lib/ghc"
executablename="$exedir/$exeprog"
if [ ! "$executablename" -ef "$libGHC/bin/$exeprog" ]; then
  $libGHC/SetupCLI $libGHC/bin || exit 1
fi
exec "$executablename" -B"$topdir" -l"$topdir" ${1+"$@"}