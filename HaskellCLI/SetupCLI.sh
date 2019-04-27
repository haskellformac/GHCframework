#!/bin/sh

#  SetupCLI.sh
#  HaskellCLI
#
#  Created by Manuel M T Chakravarty on 2/01/2016.
#  Copyright © [2016..2017] Manuel M T Chakravarty. All rights reserved.
#
#  Create hard links from the application container to the CLI tools at the location given in the first command line
#  argument (something like '/usr/local/lib/ghc-7.10.3/bin'). Links to all binaries at the latter location are created.

appContainer="$HOME/Library/Containers/com.haskellformac.Haskell.basic/Data/Library/Application Support"
appContainerGHCVersionPath="$appContainer/lib/Version"
appContainerBin="$appContainer/lib/ghc/bin"
appContainerPackageDB="$appContainer/lib/ghc/package.conf.d"
appContainerPackageCache="$appContainerPackageDB/package.cache"
if [ $# -ne 1 ]; then
  echo "$0: needs exactly one argument"
  exit 1
fi
cliLocation="$1"

appContainerIsOutdated="false"
if [ ! -d "$appContainerBin" ]; then
  appContainerIsOutdated="true"
fi

appContainerGHCVersion=`cat "$appContainerGHCVersionPath" || echo 0`
cliGHCVersion=`cat "$cliLocation/../Version"`
if [ $appContainerGHCVersion -ne $cliGHCVersion ]; then
  appContainerIsOutdated="true"
fi

appBundleRTS=`grep library-dirs "$appContainerPackageDB/rts.conf" | cut -f 2 -d ' '`
appBundleGHC=`dirname "$appBundleRTS"`
appBundlePackageCache="$appBundleGHC/package.conf.d/package.cache"

if [ ! -e "$appBundlePackageCache" ]; then
  appContainerIsOutdated="true"
fi
if [ ! -e "$appContainerPackageCache" ]; then
  appContainerIsOutdated="true"
fi
if [ "$appBundlePackageCache" -nt "$appContainerPackageCache" ]; then
  appContainerIsOutdated="true"
fi

if `$appContainerIsOutdated`; then
  echo "$0: incomplete set up"
  echo "  Before you can use the Haskell for Mac command line tools, please start"
  echo "  Haskell.app (so that it can configure the Haskell package database)."
  echo "  If that doesn't help, there is a version mismatch:"
  echo "    Haskell for Mac GHC.framework version:    $appContainerGHCVersion"
  echo "    Command line tools GHC.framework version: $cliGHCVersion"
  exit 1
fi

if [ "$cliLocation/ghc" -nt "$appContainerBin/ghc" ]; then

  # NB: We use loops instead of globbing to properly deal with path names including space characters.
  for file in `ls "$appContainerBin"`; do
    rm -f "$appContainerBin/$file"
  done
  for file in `ls "$cliLocation"`; do
    ln -f "$cliLocation/$file" "$appContainerBin"
  done
  ln -f "$appContainerBin/unlit" "$appContainerBin/.."

    # Remove the cabal.config of the old setup
  rm -f "$appContainer/cabal.config"

  mkdir -p "$appContainer/Cabal"
  sed -e "s|APPLICATION_SUPPORT|$appContainer|g" -e "s|HOME_DIR|$HOME|g" "$cliLocation/../cabal.config" >"$appContainer/Cabal/cabal.config"
  mkdir -p "$appContainer/Cabal/repo-cache"
  ditto "$cliLocation/../repo-cache" "$appContainer/Cabal/repo-cache"

fi
