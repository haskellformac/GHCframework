#!/bin/sh

#  BuildPKG.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 6/01/2016.
#  Copyright © 2020 Manuel M T Chakravarty. All rights reserved.

# Matches the GHC.framework version
GHCBUILD_CONFIGURATION_BUILD_DIR=`echo $CONFIGURATION_BUILD_DIR | sed s/HaskellCLI/GHC/`
VERSION=`plutil -extract CFBundleShortVersionString xml1 $GHCBUILD_CONFIGURATION_BUILD_DIR/GHC.framework/Resources/Info.plist -o - | grep string | sed -e 's|<string>||' -e 's|</string>||'`

FULL_PRODUCT_NAME=$CONFIGURATION_BUILD_DIR/$PRODUCT_NAME-$VERSION.pkg
CLITREE_CONTENTS=$CONFIGURATION_BUILD_DIR/CLITree.bundle/Contents
CLITREE_PKG=$CONFIGURATION_BUILD_DIR/CLI.pkg
DISTRIBUTION_PATH=$CONFIGURATION_BUILD_DIR/Distribution

if [ "x$1" == "xclean" ]; then

  rm -f $DISTRIBUTION_PATH $CLITREE_PKG $FULL_PRODUCT_NAME

else

  echo "** Build PKG"
  /usr/bin/pkgbuild --identifier com.haskellformac.pkg.CLI --version $VERSION --install-location /usr/local --root $CLITREE_CONTENTS/usr/local --scripts $CLITREE_CONTENTS/InstallScripts $CLITREE_PKG
  if [ $? -ne 0 ]; then exit 1; fi
#  echo "Synthesise"
#  /usr/bin/productbuild --identifier com.haskellformac.app.$PRODUCT_NAME --version $VERSION --synthesize --package $CLITREE_PKG $DISTRIBUTION_PATH
  echo "** Build Product"
  /usr/bin/productbuild --identifier com.haskellformac.pkg.$PRODUCT_NAME --version $VERSION --sign "Developer ID Installer" --distribution $SOURCE_ROOT/Distribution --resources $SOURCE_ROOT/Resources --package-path $CONFIGURATION_BUILD_DIR $FULL_PRODUCT_NAME

fi
