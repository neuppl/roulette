#!/bin/sh
set -e

PKGS="roulette-lib/ roulette/ gtp-util gtp-plot markdown"

for p in $PKGS; do
  raco pkg remove --auto --force "${p%/}" || true
done

raco pkg install --auto $PKGS