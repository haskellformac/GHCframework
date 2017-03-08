#!/bin/sh

bundle_id=com.haskellformac.Haskell.basic
user_scripts="/Users/${USER}/Library/Containers/${bundle_id}/Data/Library/Application Scripts/${bundle_id}"
hfm_path="HFM_PATH"

echo "Installing Haskell for Mac user scripts in ${user_scripts}"
mkdir -p "${user_scripts}"
cp -f "${hfm_path}/RunHaskellTerminal" "${user_scripts}/RunHaskellTerminal"
cp -f "${hfm_path}/RunCLICommand" "${user_scripts}/RunCLICommand"
cp -f "${hfm_path}/StopCLICommand" "${user_scripts}/StopCLICommand"
cp -f "${hfm_path}/MakeHaskellCLIDefault" "${user_scripts}/MakeHaskellCLIDefault"
cp -f "${hfm_path}/HaskellCLIStatus" "${user_scripts}/HaskellCLIStatus"
chown -f ${USER}:staff "${user_scripts}/RunHaskellTerminal"
chown -f ${USER}:staff "${user_scripts}/RunCLICommand"
chown -f ${USER}:staff "${user_scripts}/StopCLICommand"
chown -f ${USER}:staff "${user_scripts}/MakeHaskellCLIDefault"
chown -f ${USER}:staff "${user_scripts}/HaskellCLIStatus"
