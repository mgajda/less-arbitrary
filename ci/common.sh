#!/bin/bash

message () {
  echo -e "\e[1m\e[33m${*}\e[0m"
}
set -o verbose
set -o xtrace
set -o pipefail
set -o errexit
set -o nounset

export DEBIAN_FRONTEND=noninteractive
