#!/bin/sh
set -e

PKGS="roulette-lib/ roulette/ gtp-util gtp-plot markdown"

raco pkg update --auto --no-docs $PKGS || raco pkg install --auto --no-docs $PKGS
