#!/bin/sh

appContainer="$HOME/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support"
libGHC="/usr/local/lib/ghc-VERSION"
exedir="$appContainer/lib/ghc/bin"
exeprog="hsc2hs"
topdir="$appContainer/lib/ghc"
executablename="$exedir/$exeprog"
if [ ! "$executablename" -ef "$libGHC/bin/$exeprog" ]; then
  $libGHC/SetupCLI $libGHC/bin || exit 1
fi
HSC2HS_EXTRA="--cflag=-m64 --cflag=-fno-stack-protector --lflag=-m64"
tflag="--template=$topdir/template-hsc.h"
Iflag="-I$topdir/include/"
for arg do
  case "$arg" in
# On OS X, we need to specify -m32 or -m64 in order to get gcc to
# build binaries for the right target. We do that by putting it in
# HSC2HS_EXTRA. When cabal runs hsc2hs, it passes a flag saying which
# gcc to use, so if we set HSC2HS_EXTRA= then we don't get binaries
# for the right platform. So for now we just don't set HSC2HS_EXTRA=
# but we probably want to revisit how this works in the future.
#        -c*)          HSC2HS_EXTRA=;;
#        --cc=*)       HSC2HS_EXTRA=;;
    -t*)          tflag=;;
    --template=*) tflag=;;
    --)           break;;
  esac
done

exec "$executablename" ${tflag:+"$tflag"} $HSC2HS_EXTRA ${1+"$@"} "$Iflag"
