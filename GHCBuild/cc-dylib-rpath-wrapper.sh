#!/bin/bash
# NB: Needs to be 'bash' (not 'sh'), so we can use 'echo -n'.

#  cc-dylib-rpath-wrapper.sh
#  GHCBuild
#
#  Created by Manuel M T Chakravarty on 08.12.16.
#  Copyright © [2016..2017] Manuel M T Chakravarty. All rights reserved.
#
#  Same purpose as 'ghc-dylib-rpath-wrapper.sh', but for internal invocations from GHC for linking executables and
#  for TH and similar.

rsp_file=""
case $1 in
  @*) rsp_file=${1:1};;
  *) ;;
esac

if [ -n "$rsp_file" ]; then

#  cat $rsp_file           >&2
#  echo "------------"     >&2

  eval args=(`cat $rsp_file`)
  echo -n ""                       >$rsp_file
  rpath=""
  final_embed_rpath=""
  final_container_rpath=""
  loader_rpath=""
  include_rpath=true
  for arg in "${args[@]}"; do
    case $arg in
      -Wl,-r)
        echo "\"-Wl,-r\""    >>$rsp_file
        include_rpath=false;;
      -Wl,-rpath)
        rpath="rpath";;
      -Wl,/*usr/lib/ghc*/*)
        rpath=""
        final_embed_rpath=`dirname "${arg:4}"`;;
      -Wl,-rpath,/*usr/lib/ghc*/*)
        final_embed_rpath=`dirname ${arg:11}`;;
      -Wl,/*Support/lib/ghc*/*)
        rpath=""
        final_container_rpath=`dirname "${arg:4}"`;;
      -Wl,-rpath,/*Support/lib/ghc*/*)
        final_container_rpath=`dirname "${arg:11}"`;;
      -rpath)
        rpath="rpath_cc";;
      @loader_path/..)
        rpath=""
        loader_rpath="${arg}";;
      -L/*Application_Support/lib/ghc*/*)
        # Crude hack to get around 'cabal' infelicity with spaces in file names
        arg_space="`echo "$arg" | sed -e 's/Application_Support/Application Support/'`"
        echo "\"$arg_space\""        >>$rsp_file
        final_container_rpath=`dirname "${arg_space:2}"`;;
      -L/*Support/lib/ghc*/*)
        echo "\"$arg\""            >>$rsp_file
        final_container_rpath=`dirname "${arg:2}"`;;
      *)
        if [ "x$rpath" = "xrpath" ]; then
          echo "\"-Wl,-rpath\""    >>$rsp_file
          rpath=""
        elif [ "x$rpath" = "xrpath_cc" ]; then
          echo "\"-rpath\""        >>$rsp_file
          rpath=""
        fi
        echo "\"$arg\""            >>$rsp_file
        ;;
    esac
  done
  if `$include_rpath`; then
    if [ -n "$final_embed_rpath" ]; then
      echo "\"-Wl,-rpath\""        >>$rsp_file
      echo "\"-Wl,${final_embed_rpath}\"" >>$rsp_file
    fi
    if [ -n "$final_container_rpath" ]; then
      echo "\"-Wl,-rpath\""        >>$rsp_file
      echo "\"-Wl,${final_container_rpath}\"" >>$rsp_file
    fi
    if [ -n "$loader_rpath" ]; then
      echo "\"-rpath\""            >>$rsp_file
      echo "\"@loader_path/..\""   >>$rsp_file
    fi
  fi

#  cat $rsp_file           >&2
fi
exec /usr/bin/gcc ${1+"$@"}
