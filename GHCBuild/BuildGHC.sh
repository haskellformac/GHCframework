#!/bin/sh

#  BuildGHC.sh
#  GHCBuild
#
#  Created by Manuel M T Chakravarty on 07.12.16.
#  Copyright © [2016..2019] Manuel M T Chakravarty. All rights reserved.

TARGET_TEMP_BINDIR="$TARGET_TEMP_DIR/bin"
mkdir -p "$TARGET_TEMP_BINDIR"
export PATH="$TARGET_TEMP_BINDIR:$PATH"

# Determine a version of nm suitable for the GHC build system
NM=`which nm-classic`
if [ $? -ne 0 ]; then
  NM=`which nm`
fi

# Install location
PREFIX=$CONFIGURATION_BUILD_DIR/$CONTENTS_FOLDER_PATH/usr

echo "Install location $PREFIX"

# Clean to recompile
if [ -x ${PREFIX}/bin/ghc ]; then
  exit 0
fi

# Put link to all tools into a bin/ directory in the build arena
ln -sf /Library/Haskell/bin/cabal    $TARGET_TEMP_BINDIR/cabal
ln -sf /Library/Haskell/bin/alex     $TARGET_TEMP_BINDIR/alex
ln -sf /Library/Haskell/bin/happy    $TARGET_TEMP_BINDIR/happy
ln -sf /Library/Haskell/bin/HsColour $TARGET_TEMP_BINDIR/HsColour
ln -sf /Library/Frameworks/GHC.framework/Versions/Current/usr/bin/ghc $TARGET_TEMP_BINDIR/ghc
ln -sf /Library/Frameworks/GHC.framework/Versions/Current/usr/bin/ghc-pkg $TARGET_TEMP_BINDIR/ghc-pkg
ln -sf /Library/Frameworks/GHC.framework/Versions/Current/usr/bin/hsc2hs $TARGET_TEMP_BINDIR/hsc2hs

# Compile 'lndir' to create the build tree
(cd GHCBuild/ghc/utils/lndir/; clang -I../fs -O -o $TARGET_TEMP_BINDIR/lndir lndir.c ../fs/fs.c)
mkdir -p $TARGET_TEMP_DIR/ghc/
#(cd $TARGET_TEMP_DIR/ghc/; lndir $SOURCE_ROOT/GHCBuild/ghc >/dev/null; ln -sf $SOURCE_ROOT/GHCBuild/ghc/.git $TARGET_TEMP_DIR/ghc/)
(cd $TARGET_TEMP_DIR/ghc/; lndir $SOURCE_ROOT/GHCBuild/ghc >/dev/null; rm -f .git)

# Select the appropriate 'build.mk' setup
if [ $CONFIGURATION = "Debug" ];
then
  cp -f $SOURCE_ROOT/GHCBuild/build.mk.debug $TARGET_TEMP_DIR/ghc/mk/build.mk
else
  cp -f $SOURCE_ROOT/GHCBuild/build.mk.release $TARGET_TEMP_DIR/ghc/mk/build.mk
fi

# Actual GHC build (we extend PATH to get 'autoreconf')
echo "Building in $TARGET_TEMP_DIR/ghc"
#echo "8.6.4" >$TARGET_TEMP_DIR/ghc/VERSION
(cd $TARGET_TEMP_DIR/ghc; env PATH=$PATH:/usr/local/bin perl boot ) || exit 1
(cd $TARGET_TEMP_DIR/ghc; ./configure --prefix=$PREFIX --with-nm=$NM) || exit 1
(cd $TARGET_TEMP_DIR/ghc; make -j3) || exit 1
(cd $TARGET_TEMP_DIR/ghc; make install) || exit 1

# Create a link to the library directory that is not versioned.
# (We only ever have got one version in the framework and this simplifies other scripts.)
GHC_VERSION=`${PREFIX}/bin/ghc --numeric-version`
(cd ${PREFIX}/lib; ln -hfs ghc-${GHC_VERSION} ghc)

## Adjust library names and RPATHs for packages bundled with GHC (this is still somewhat different to the set up of
## packages installed subsequently).

GHCBASE=$CONFIGURATION_BUILD_DIR/$CONTENTS_FOLDER_PATH/usr
GHCBIN=$GHCBASE/bin
GHC_WRAPPER=GHCBuild/ghc-dylib-rpath-wrapper.sh

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

# Change the dylib install names of all GHC packages (except the RTS) to relocatable ones.
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

