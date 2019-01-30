#!/bin/bash
#
# trys to evaluate all nixpkgs and puts them in folder ./store
# information about each derivation is written into a file store_info.txt
#
NIX_STORE_DIR="`pwd`/store" NIX_STATE_DIR=/tmp/nix/var/nix/ nix-instantiate all.nix -A filtered
NIX_STORE_DIR="`pwd`/store" NIX_STATE_DIR=/tmp/nix/var/nix/ echo -e `nix-instantiate --eval all.nix -A storeFile` > store-info.txt
