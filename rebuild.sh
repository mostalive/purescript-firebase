#!/bin/bash

# Bash Strict Mode http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

./clean.sh
rm -rf node_modules
rm -rf bower_components
npm install
bower install
pulp test

