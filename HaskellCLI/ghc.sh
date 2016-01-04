#!/bin/sh

appContainer="/Users/$USER/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support"
libGHC="/usr/local/lib/ghc-VERSION"
exedir="$appContainer/lib/ghc/bin"
exeprog="ghc"
topdir="$appContainer/lib/ghc"
executablename="$exedir/$exeprog"
if [ ! "$executablename" -ef "$libGHC/bin/$exeprog" ]; then
  $libGHC/SetupCLI $libGHC/bin
fi
exec "$executablename" -B"$topdir" -dynamic -optl -rpath -optl "$topdir" ${1+"$@"}