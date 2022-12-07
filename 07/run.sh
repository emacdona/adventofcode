#!/usr/bin/env bash
set -eo pipefail

g++ main.cpp && cat input.txt | ./a.out
