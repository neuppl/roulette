#!/bin/sh
set -e

PKGS="roulette-lib/ roulette/ gtp-util gtp-plot markdown"

for p in $PKGS; do
  raco pkg update --auto "${p%/}" || raco pkg install --auto "${p%/}"
done
