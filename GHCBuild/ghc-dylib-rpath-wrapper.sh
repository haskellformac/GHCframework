#!/bin/sh

#  ghc-dylib-rpath-wrapper.sh
#  GHCBuild
#
#  Created by Manuel M T Chakravarty on 07.12.16.
#  Copyright © [2016..2017] Manuel M T Chakravarty. All rights reserved.
#
#  Rewrite ghc invocations by cabal install that build dynamic libraries (for a Haskell package) such that the library
#  name is relative to $GHCLIB and that we avoid the individual RPATHs for each library this package depends on.

declare -a args
i=0
package_key=""
dylib=""
while [ $# -gt 0 ]; do
  arg="$1"
  shift
  case "$arg" in
    -optl-Wl,-rpath*/lib/ghc*)
      continue;;
    -this-package-key)
      if [ $# -gt 0 ]; then
        package_key="$1"
      fi;;
    -o)
      if [ $# -gt 0 ]; then
        case "$1" in
          *.dylib)
            dylib=`basename "$1"`;;
          *) ;;
        esac
      fi;;
    *) ;;
  esac
  args[i]="$arg"
  i=$(( $i + 1 ))
done

if [ -n "$package_key" -a -n "$dylib" ]; then
  args[i]="-optl-Wl,-install_name,@rpath/$package_key/$dylib"
fi

GHCBASE=$CONFIGURATION_BUILD_DIR/$CONTENTS_FOLDER_PATH/usr
GHCBIN=$GHCBASE/bin

#echo $GHCBIN/ghc "${args[@]}" >&2
exec $GHCBIN/ghc "${args[@]}"
