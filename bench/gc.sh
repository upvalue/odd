#!/bin/bash
# run from toplevel directory
set -x
rm -f bench/gc-pip bench/gc-boehm

g++ -I. -O3 -fwhole-program -o bench/gc-pip bench/gc.cpp
g++ -I. -DBOEHM_GC=1 -O3 -fwhole-program -o bench/gc-boehm bench/gc.cpp -lgc

time bench/gc-pip
time bench/gc-boehm 
