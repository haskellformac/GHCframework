#!/bin/sh

appContainer="$HOME/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support"
libGHC="/usr/local/lib/ghc-VERSION"
exedir="$appContainer/lib/ghc/bin"
exeprog="ghc-pkg"
topdir="$appContainer/lib/ghc"
executablename="$exedir/$exeprog"
if [ ! "$executablename" -ef "$libGHC/bin/$exeprog" ]; then
  $libGHC/SetupCLI $libGHC/bin
fi
PKGCONF="$topdir/package.conf.d"
exec "$executablename" --global-package-db "$PKGCONF" ${1+"$@"}