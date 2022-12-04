#!/usr/bin/env bash
set -eo pipefail

# Scala 3 required
cat input.txt | scala ./advent04.scala
