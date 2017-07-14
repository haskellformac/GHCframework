#!/bin/sh

#  BuildContainer.sh
#  GHCBuild
#
#  Created by Manuel M T Chakravarty on 12.07.17.
#  Copyright © 2017 Manuel M T Chakravarty. All rights reserved.

VERSION=`plutil -extract CFBundleShortVersionString xml1 ${SOURCE_ROOT}/GHC/Info.plist -o - | grep string | sed -e 's|<string>||' -e 's|</string>||'`
GHC_VERSION=`echo ${VERSION} |  cut -d '-' -f1`
LTS_VERSION=`echo ${VERSION} |  cut -d '-' -f2`
echo "GHC.framework version = ${VERSION}"

echo "Docker"
docker version

echo "GHC"
docker run -t --rm haskell:${GHC_VERSION} ghc --info

docker build --tag=ghc_framework:${VERSION} -f ${SOURCE_ROOT}/GHCBuild/Dockerfile ${SOURCE_ROOT}/GHCBuild
