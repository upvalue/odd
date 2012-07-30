#!/bin/bash
# run from toplevel directory
set -x
rm -f bench/gc-pip bench/gc-boehm

CFLAGS="-std=c++0x -g3 -O3"

g++ -I. $CFLAGS -o bench/gc-pip bench/gc.cpp
g++ -I. -DBOEHM_GC=1 $CFLAGS -o bench/gc-boehm bench/gc.cpp -lgc

time bench/gc-pip
time bench/gc-boehm 
