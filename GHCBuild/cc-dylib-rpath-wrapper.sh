#!/bin/bash
# NB: Needs to be 'bash' (not 'sh'), so we can use 'echo -n'.

#  cc-dylib-rpath-wrapper.sh
#  GHCBuild
#
#  Created by Manuel M T Chakravarty on 08.12.16.
#  Copyright © 2016 Manuel M T Chakravarty. All rights reserved.

rsp_file=""
case $1 in
  @*) rsp_file=${1:1};;
  *) ;;
esac

if [ -n $rsp_file ]; then

args=`cat $rsp_file`
echo -n ""                       >$rsp_file
rpath=""
final_rpath=""
for arg in $args; do
  case $arg in
    \"-Wl,-rpath\")
      rpath="rpath";;
    \"-Wl,/*usr/lib/ghc*\")
      rpath=""
      final_rpath=`dirname ${arg:5}`;;
    *)
      if [ "x$rpath" = "xrpath" ]; then
        echo "\"-Wl,-rpath\""   >>$rsp_file
        rpath=""
      fi
      echo $arg                 >>$rsp_file
      ;;
  esac
done
if [ -n "$final_rpath" ]; then
  echo "\"-Wl,-rpath\""         >>$rsp_file
  echo "\"${final_rpath}\""     >>$rsp_file
fi

cat $rsp_file           >&2
fi
exec /usr/bin/gcc ${1+"$@"}
