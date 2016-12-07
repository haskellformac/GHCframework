#!/bin/sh

#  InstallPackages.sh
#  GHCBuild
#
#  Created by Manuel M T Chakravarty on 07.12.16.
#  Copyright © 2016 Manuel M T Chakravarty. All rights reserved.

GHCBASE=$CONFIGURATION_BUILD_DIR/$CONTENTS_FOLDER_PATH/usr
GHCBIN=$GHCBASE/bin

GHC_VERSION=`$GHCBIN/ghc --numeric-version`

GHCLIB=$GHCBASE/lib/ghc-${GHC_VERSION}
GHCSHARE=$GHCBASE/share

echo "Using --package-db=$GHCLIB/package.conf.d"

# Change the dylib install names of the rts to relocatable ones. We handle the rts separately
# as it contains multiple dylibs that we need to rewrite (including also libffi.dylib).
RTSDIR=`$GHCBIN/ghc-pkg --package-db=$GHCLIB/package.conf.d field rts key | cut -f 2 -d ' '`
echo "Fix install names and rpaths: $RTSDIR/lib*.dylib"
for DYLIBPATH in `ls $GHCLIB/$RTSDIR/lib*.dylib`; do
  echo -n "$DYLIBPATH..."
  install_name_tool -id "@rpath/$RTSDIR/`basename $DYLIBPATH`" $DYLIBPATH
  if `otool -l $DYLIBPATH | grep -q "@loader_path/.."`; then
    echo "rpath ok"
  else
    install_name_tool -add_rpath "@loader_path/.." $DYLIBPATH
  echo "done"
  fi
done

# Install further packages
PKGS=`cat $SOURCE_ROOT/GHCBuild/extra-packages`
if [ $CONFIGURATION = "Debug" ];
then
  EXTRA_ARGS="$EXTRA_ARGS --disable-documentation"
else
  EXTRA_ARGS="$EXTRA_ARGS --with-haddock=$GHCBIN/haddock"
fi
# Current cabal version doesn't let us leave out the global or user package DB. We
# must use --reinstall to avoid that an existing package in the global or user DB with
# the same version suppresses the installation.
CABAL_CMD="/Library/Haskell/bin/cabal --config-file=$SOURCE_ROOT/GHCBuild/cabal.config install -j --prefix=$GHCLIB --bindir=$GHCLIB/bin --libdir=$GHCLIB --libexecdir=$GHCLIB/libexec --datadir=$GHCSHARE --package-db=$GHCLIB/package.conf.d --with-compiler=$GHCBIN/ghc --with-hc-pkg=$GHCBIN/ghc-pkg --with-alex=/Library/Haskell/bin/alex --with-happy=/Library/Haskell/bin/happy --allow-newer --ghc-options=-optl-Wl,-headerpad_max_install_names $EXTRA_ARGS"
echo "$CABAL_CMD <PACKAGE LIST>"
$CABAL_CMD $PKGS

# Change the dylib install names of all packages (except the RTS) to relocatable ones.
PKGS=`$GHCBIN/ghc-pkg --package-db=$GHCLIB/package.conf.d list --simple-output`
for PKG in $PKGS; do
  ID=`$GHCBIN/ghc-pkg --package-db=$GHCLIB/package.conf.d field $PKG id | cut -f 2 -d ' '`
  KEY=`$GHCBIN/ghc-pkg --package-db=$GHCLIB/package.conf.d field $PKG key | cut -f 2 -d ' '`
  HSLIB=`$GHCBIN/ghc-pkg --package-db=$GHCLIB/package.conf.d field $PKG hs-libraries | cut -f 2 -d ' '`
  LIBPATH=`$GHCBIN/ghc-pkg --package-db=$GHCLIB/package.conf.d field $PKG library-dirs | cut -f 2 -d ' '`
  if [ "x$LIBPATH" != "x" -a $KEY != $RTSDIR ]; then
    NAME=lib${HSLIB}-ghc${GHC_VERSION}.dylib
    DIR=`basename $LIBPATH`
    if [ `otool -D $LIBPATH/$NAME | tail -1` != "@rpath/$DIR/$NAME" ]; then
      echo "Fix install name and rpath: $DIR/$NAME (key: $KEY)"
      install_name_tool -id "@rpath/$DIR/$NAME" $LIBPATH/$NAME
      if [ $? -ne 0 ]; then exit 1; fi
      if otool -l $LIBPATH/$NAME | grep -q 'path @loader_path/..'; then
        echo "Already has @loader_path/.."
      else
        install_name_tool -add_rpath "@loader_path/.." $LIBPATH/$NAME
        if [ $? -ne 0 ]; then exit 1; fi
      fi
      # Get rid of absolute RPATHs (clutter & hiding bugs in debug w/o sandboxing) and add
      # an RPATH relative to @loader_path including the directory of the *loaded* dylib.
      for path in `otool -l $LIBPATH/$NAME | grep ' path ' | grep DerivedData | cut -d ' ' -f 11`; do
        install_name_tool -delete_rpath $path $LIBPATH/$NAME
        install_name_tool -add_rpath "@loader_path/../`basename $path`" $LIBPATH/$NAME
      done
    fi
  fi
done

for path in `otool -l $GHCLIB/bin/cpphs | grep ' path ' | grep DerivedData | cut -d ' ' -f 11`; do
  install_name_tool -delete_rpath $path $GHCLIB/bin/cpphs
  install_name_tool -add_rpath "@loader_path/../`basename $path`" $GHCLIB/bin/cpphs
done
# FIXME: Instead of adding all the "@loader_path/../$PKG" RPATHs, it would be better if we
#        could have the $PKG in the install name right from the start. Currently, that is
#        hard as it is too late by the time Cabal is done. Can we adapt Cabal?

# We build the executables separately, so they already get the RPATHs and names of the
# relocatable libs.
$CABAL_CMD alex happy cabal-install

# Remove absolute RPATHs embedded in the binaries
BINS="alex cabal happy"
for BIN in $BINS; do
  for path in `otool -l $GHCLIB/bin/$BIN | grep ' path ' | grep DerivedData | cut -d ' ' -f 11`; do
    install_name_tool -delete_rpath $path $GHCLIB/bin/$BIN
  done
done

# We don't want sample etc binaries of library packages.
rm -f $GHCLIB/bin/operational-TicTacToe
rm -f $GHCLIB/bin/aeson-pretty
rm -f $GHCLIB/bin/mkReadme
