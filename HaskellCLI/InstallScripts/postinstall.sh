#!/bin/sh

bundle_id=com.haskellformac.Haskell.basic
user_scripts="/Users/${USER}/Library/Containers/${bundle_id}/Data/Library/Application Scripts/${bundle_id}"
hfm_path="HFM_PATH"

echo "Installing Haskell for Mac user scripts in ${user_scripts}"
mkdir -p "${user_scripts}"
cp -f "${hfm_path}/RunHaskellTerminal" "${user_scripts}/RunHaskellTerminal"
