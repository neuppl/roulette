#!/bin/sh
set -e

PKGS="roulette-lib/ roulette/ gtp-util gtp-plot markdown"

raco pkg update --auto $PKGS || raco pkg install --auto $PKGS
