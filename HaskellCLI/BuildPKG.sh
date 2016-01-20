#!/bin/sh

#  BuildPKG.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 6/01/2016.
#  Copyright © 2016 Manuel M T Chakravarty. All rights reserved.

# FIRST TWO MUST MATCH HfM APP — third one is package version
VERSION=1.1.0

FULL_PRODUCT_NAME=$CONFIGURATION_BUILD_DIR/$PRODUCT_NAME.pkg
CLITREE_CONTENTS=$CONFIGURATION_BUILD_DIR/CLITree.bundle/Contents
CLITREE_PKG=$CONFIGURATION_BUILD_DIR/CLI.pkg
DISTRIBUTION_PATH=$CONFIGURATION_BUILD_DIR/Distribution

if [ "x$1" == "xclean" ]; then

  rm -f $DISTRIBUTION_PATH $CLITREE_PKG $FULL_PRODUCT_NAME

else

  echo "** Build PKG"
  /usr/bin/pkgbuild --identifier com.haskellformac.pkg.CLI --version $VERSION --install-location /usr/local --root $CLITREE_CONTENTS/usr/local $CLITREE_PKG
#  echo "Synthesise"
#  /usr/bin/productbuild --identifier com.haskellformac.app.$PRODUCT_NAME --version $VERSION --synthesize --package $CLITREE_PKG $DISTRIBUTION_PATH
  echo "** Build Product"
  /usr/bin/productbuild --identifier com.haskellformac.pkg.$PRODUCT_NAME --version $VERSION --sign "Developer ID Installer" --distribution $SOURCE_ROOT/Distribution --resources $SOURCE_ROOT/Resources --package-path $CONFIGURATION_BUILD_DIR $FULL_PRODUCT_NAME

fi
