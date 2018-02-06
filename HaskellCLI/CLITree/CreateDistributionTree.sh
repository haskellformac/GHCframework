#!/bin/sh

#  CreateDistributionTree.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 07.02.17.
#  Copyright © [2017..2018] Manuel M T Chakravarty. All rights reserved.


GHCBUILD_CONFIGURATION_BUILD_DIR=`echo $CONFIGURATION_BUILD_DIR | sed s/HaskellCLI/GHC/`
GHCROOT=$GHCBUILD_CONFIGURATION_BUILD_DIR/GHCBuild.bundle/Contents
GHCBIN=$GHCROOT/usr/bin
GHC_VERSION=`$GHCBIN/ghc --numeric-version`
CLI_VERSION=`plutil -extract CFBundleShortVersionString xml1 $SOURCE_ROOT/../GHC/Info.plist -o - | grep string | sed -e 's|<string>||' -e 's|</string>||'`
LTS_VERSION=`echo ${CLI_VERSION} | cut -d '-' -f2`

echo "GHCROOT = $GHCROOT; GHC_VERSION = $GHC_VERSION; CLI_VERSION = $CLI_VERSION"

# This is where we create the destination tree
# GHC_CONTENTS: components of GHC toolchain
# BIN_CONTENTS: launch scripts for the CLI tools
# HFM_CONTENTS: scripts for HfM user script folder
# INSTALL_CONTENTS_PATH: scripts for the package installer
CONTENTS_PREFIX=${CONFIGURATION_BUILD_DIR}/${CONTENTS_FOLDER_PATH}
LIB_PREFIX=/usr/local/lib/HaskellCLI-${CLI_VERSION}

GHC_CONTENTS_PATH=${CONTENTS_PREFIX}${LIB_PREFIX}/ghc

BIN_PATH=${LIB_PREFIX}/bin
BIN_CONTENTS_PATH=${CONTENTS_PREFIX}${BIN_PATH}

HFM_PATH=${LIB_PREFIX}/hfm
HFM_CONTENTS_PATH=${CONTENTS_PREFIX}${HFM_PATH}

INSTALL_CONTENTS_PATH=${CONTENTS_PREFIX}/InstallScripts

mkdir -p ${BIN_CONTENTS_PATH}
mkdir -p ${GHC_CONTENTS_PATH}/bin
mkdir -p ${HFM_CONTENTS_PATH}
mkdir -p ${INSTALL_CONTENTS_PATH}

# Scripts in ${LIB_PREFIX}/bin
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/ghc.sh >${BIN_CONTENTS_PATH}/ghc-$GHC_VERSION
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/ghci.sh >${BIN_CONTENTS_PATH}/ghci-$GHC_VERSION
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/runghc.sh >${BIN_CONTENTS_PATH}/runghc-$GHC_VERSION
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/ghc-pkg.sh >${BIN_CONTENTS_PATH}/ghc-pkg-$GHC_VERSION
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/haddock.sh >${BIN_CONTENTS_PATH}/haddock-$GHC_VERSION
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/alex.sh >${BIN_CONTENTS_PATH}/alex
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/happy.sh >${BIN_CONTENTS_PATH}/happy
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/cabal.sh >${BIN_CONTENTS_PATH}/cabal
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/hpc.sh >${BIN_CONTENTS_PATH}/hpc
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/hsc2hs.sh >${BIN_CONTENTS_PATH}/hsc2hs
sed -e "s/VERSION/$CLI_VERSION/g" $SOURCE_ROOT/cpphs.sh >${BIN_CONTENTS_PATH}/cpphs
cat $SOURCE_ROOT/../GHCBuild/cc-dylib-rpath-wrapper.sh >${BIN_CONTENTS_PATH}/cc-dylib-rpath-wrapper
sed -e "s|\$CONFIGURATION_BUILD_DIR/\$CONTENTS_FOLDER_PATH/usr|${LIB_PREFIX}|g" $SOURCE_ROOT/../GHCBuild/ghc-dylib-rpath-wrapper.sh >${BIN_CONTENTS_PATH}/ghc-dylib-rpath-wrapper
chmod a+x ${BIN_CONTENTS_PATH}/ghc-$GHC_VERSION
chmod a+x ${BIN_CONTENTS_PATH}/ghci-$GHC_VERSION
chmod a+x ${BIN_CONTENTS_PATH}/runghc-$GHC_VERSION
chmod a+x ${BIN_CONTENTS_PATH}/ghc-pkg-$GHC_VERSION
chmod a+x ${BIN_CONTENTS_PATH}/haddock-$GHC_VERSION
chmod a+x ${BIN_CONTENTS_PATH}/alex
chmod a+x ${BIN_CONTENTS_PATH}/happy
chmod a+x ${BIN_CONTENTS_PATH}/cabal
chmod a+x ${BIN_CONTENTS_PATH}/hpc
chmod a+x ${BIN_CONTENTS_PATH}/hsc2hs
chmod a+x ${BIN_CONTENTS_PATH}/cpphs
chmod a+x ${BIN_CONTENTS_PATH}/cc-dylib-rpath-wrapper
chmod a+x ${BIN_CONTENTS_PATH}/ghc-dylib-rpath-wrapper

cp -f $GHCROOT/usr/bin/hp2ps ${BIN_CONTENTS_PATH}
cp -f $GHCROOT/usr/bin/stack ${BIN_CONTENTS_PATH}
cp -f $GHCROOT/usr/lib/ghc/bin/c2hs ${BIN_CONTENTS_PATH}

# Links in ${LIB_PREFIX}/bin
ln -hfs ghc-$GHC_VERSION ${BIN_CONTENTS_PATH}/ghc
ln -hfs ghci-$GHC_VERSION ${BIN_CONTENTS_PATH}/ghci
ln -hfs runghc-$GHC_VERSION ${BIN_CONTENTS_PATH}/runghc
ln -hfs runghc ${BIN_CONTENTS_PATH}/runhaskell
ln -hfs ghc-pkg-$GHC_VERSION ${BIN_CONTENTS_PATH}/ghc-pkg
if [ $CONFIGURATION = "Release" ]; then
  ln -hfs haddock-$GHC_VERSION ${BIN_CONTENTS_PATH}/haddock
fi

# Scripts in ${LIB_PREFIX}/ghc
cp -f $SOURCE_ROOT/SetupCLI.sh ${GHC_CONTENTS_PATH}/SetupCLI
chmod a+x ${GHC_CONTENTS_PATH}/SetupCLI

cp -f $GHCROOT/usr/lib/ghc/bin/ghc ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/runghc ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/ghc-pkg ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/ghc-split ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/ghc-iserv ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/ghc-iserv-dyn ${GHC_CONTENTS_PATH}/bin
if [ $CONFIGURATION = "Release" ]; then
  cp -f $GHCROOT/usr/lib/ghc/bin/haddock ${GHC_CONTENTS_PATH}/bin
fi
cp -f $GHCROOT/usr/lib/ghc/bin/hpc ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/hsc2hs ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/unlit ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/alex ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/happy ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/cabal ${GHC_CONTENTS_PATH}/bin
cp -f $GHCROOT/usr/lib/ghc/bin/cpphs ${GHC_CONTENTS_PATH}/bin


# Build cabal.config
echo -n "-- Haskell for Mac CLI for GHC.framework "                                  >${GHC_CONTENTS_PATH}/cabal.config
echo ${CLI_VERSION}                                                                 >>${GHC_CONTENTS_PATH}/cabal.config
grep '\-- remote-repo:' ${SOURCE_ROOT}/../GHCBuild/cabal.config | sed -e 's/-- //'  >>${GHC_CONTENTS_PATH}/cabal.config
sed -e "s/VERSION/$CLI_VERSION/g" ${SOURCE_ROOT}/Cabal/cabal.config                 >>${GHC_CONTENTS_PATH}/cabal.config

# Populate repo-cache
CABAL_FILE_TREE="${PROJECT_TEMP_DIR}/cabal"
mkdir -p ${GHC_CONTENTS_PATH}/repo-cache/stackage-lts-${LTS_VERSION}
cp -f ${SOURCE_ROOT}/Cabal/00-index.* ${GHC_CONTENTS_PATH}/repo-cache/stackage-lts-${LTS_VERSION}
rm -rf "${CABAL_FILE_TREE}"
mkdir -p "${CABAL_FILE_TREE}"
tar -C "${CABAL_FILE_TREE}" -xf ${SOURCE_ROOT}/Cabal/00-index.tar.gz

echo -n "" >${GHC_CONTENTS_PATH}/repo-cache/stackage-lts-${LTS_VERSION}/00-index.summary
for cabal in ${CABAL_FILE_TREE}/*/*/*.cabal; do
  sed -E -e ':a' -e 'N' -e '$!ba' -e 's/[[:<:]]name:.?\n/name: /' -e 's/[[:<:]]version:.?\n/version: /' -e 's/[[:<:]]synopsis:.?\n/synopsis: /' ${cabal} \
  | grep -i -e '^name:' -e '^version:' -e '^synopsis:' \
  >>${GHC_CONTENTS_PATH}/repo-cache/stackage-lts-${LTS_VERSION}/00-index.summary
done

# Embed build version to enable accurate version check by HfM
if [ $CONFIGURATION = "Release" ]; then
  GhcInfoPlist="${PROJECT_TEMP_DIR}/../../../GHC/BuildProductsPath/Release/GHC.framework/Resources/Info.plist"
else
  GhcInfoPlist="${TARGET_BUILD_DIR}/GHC.framework/Resources/Info.plist"
fi
plutil -extract CFBundleVersion xml1 "$GhcInfoPlist" -o - | grep string | sed -e 's|<string>||' -e 's|</string>||' >${GHC_CONTENTS_PATH}/Version

# Scripts in ${LIB_PREFIX}/hfm
sed -e "s|BIN_PATH|${BIN_PATH}|g" ${SOURCE_ROOT}/RunHaskellTerminal.sh >${HFM_CONTENTS_PATH}/RunHaskellTerminal
sed -e "s|BIN_PATH|${BIN_PATH}|g" ${SOURCE_ROOT}/RunCLICommand.sh >${HFM_CONTENTS_PATH}/RunCLICommand
sed -e "s|BIN_PATH|${BIN_PATH}|g" ${SOURCE_ROOT}/StopCLICommand.sh >${HFM_CONTENTS_PATH}/StopCLICommand
sed -e "s|BIN_PATH|${BIN_PATH}|g" ${SOURCE_ROOT}/MakeHaskellCLIDefault.sh >${HFM_CONTENTS_PATH}/MakeHaskellCLIDefault
sed -e "s|BIN_PATH|${BIN_PATH}|g" ${SOURCE_ROOT}/HaskellCLIStatus.sh >${HFM_CONTENTS_PATH}/HaskellCLIStatus
chmod a+x ${HFM_CONTENTS_PATH}/RunHaskellTerminal
chmod a+x ${HFM_CONTENTS_PATH}/RunCLICommand
chmod a+x ${HFM_CONTENTS_PATH}/StopCLICommand
chmod a+x ${HFM_CONTENTS_PATH}/MakeHaskellCLIDefault
chmod a+x ${HFM_CONTENTS_PATH}/HaskellCLIStatus

# Copy the install scripts for the installer package in its own subtree
sed -e "s|HFM_PATH|${HFM_PATH}|g" ${SOURCE_ROOT}/InstallScripts/postinstall.sh >${INSTALL_CONTENTS_PATH}/postinstall
chmod a+x ${INSTALL_CONTENTS_PATH}/postinstall
