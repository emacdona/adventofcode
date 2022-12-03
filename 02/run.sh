#!/usr/bin/env bash
set -eo pipefail

cat input.txt | ./gradlew run
