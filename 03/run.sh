#!/usr/bin/env bash
set -eo pipefail

gcc main.c && cat input.txt | ./a.out
