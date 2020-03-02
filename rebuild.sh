#!/bin/bash

# run `nvm use` before starting this script to set the common node version
# https://github.com/nvm-sh/nvm/blob/master/README.md
# Bash Strict Mode http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'


./clean.sh
rm -rf node_modules
rm -rf bower_components
npm install
npx bower install
npm run build

