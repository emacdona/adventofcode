#!/usr/bin/env bash

cat input.txt | ./sum.pl | sort -n | tail -n 1
