// gc.cpp - This is a simple GC benchmark derived from one I found on
// the Boehm GC page

#ifndef BOEHM_GC
# define BOEHM_GC 0
#endif

#define PIP_LINK
#define PIP_GC_STATS 1

#include <iostream>
#include <sys/time.h>

#include "pip.hpp"

using namespace pip;
using namespace std;

#if BOEHM_GC
# include "gc.h"
# include "gc/gc_cpp.h"
#endif

// Constants
static const int kStretchTreeDepth    = 20;      // about 16Mb
static const int kLongLivedTreeDepth  = 16;  // about 4Mb
static const int kArraySize  = 500000;  // about 4Mb
static const int kMinTreeDepth = 4;
static const int kMaxTreeDepth = 20;

// Time tracking
unsigned
stats_rtclock( void )
{
  struct timeval t;
  struct timezone tz;

  if (gettimeofday( &t, &tz ) == -1)
    return 0;
  return (t.tv_sec * 1000 + t.tv_usec / 1000);
}

#define CURRENT_TIME() stats_rtclock()

// Utilities
static int tree_size(int i) {
  return ((1 << (i + 1)) - 1);
}

static int numiters(int i) {
  return 2 * tree_size(kStretchTreeDepth) / tree_size(i);
}

// Actual benchmarks

#if BOEHM_GC

struct GCPair : public Pair, gc {

  GCPair(GCPair *car_ = 0, GCPair *cdr_ = 0) {
    car = car_;
    cdr = cdr_;
  }
};

static void populate(int depth, GCPair* node) {
  if(depth <= 0) return;
  depth--;
  node->car = new GCPair();
  node->cdr = new GCPair();
  populate(depth, (GCPair*)node->car);
  populate(depth, (GCPair*)node->cdr);
}

static GCPair* make_tree(int depth) {
  if(depth <= 0) {
    return new GCPair();
  } else {
    return new GCPair(make_tree(depth-1),
                      make_tree(depth-1));
  }
}

static void time_construction(int depth) {
  long start;
  int iterations = numiters(depth);
  GCPair* temp_tree;
  long td_time = 0, bu_time = 0;
  
  start = CURRENT_TIME();
  for(int i = 0; i < iterations; ++i) {
    temp_tree = new GCPair();
    populate(depth, temp_tree);
  }
  td_time = CURRENT_TIME() - start;
  start = CURRENT_TIME();
  for(int i = 0; i < iterations; ++i) {
    temp_tree = make_tree(depth);
  }
  bu_time = CURRENT_TIME() - start;
  cout << "> Created " << iterations << " trees of depth " << depth;
  cout << " (top down: " << td_time << "ms" << " bottom up: " << bu_time << "ms)" << endl;
}

static void bench_boehm() {
  GCPair *root, *long_lived_tree, *temp_tree;
  temp_tree = make_tree(kStretchTreeDepth);
  temp_tree = 0;
  // Create long-lived array
  cout << "! Creating a long-lived array of " << kArraySize << " doubles " << endl;
  double* array = (double*) GC_MALLOC_ATOMIC(sizeof(double) * kArraySize);
  for(int i = 0; i < kArraySize / 2; ++i) {
    array[i] = 1.0/i;
  }
  // Create long-lived tree
  cout << "! Creating a long-lived binary tree of depth " << kLongLivedTreeDepth << endl;
  long_lived_tree = new GCPair();
  populate(kLongLivedTreeDepth, long_lived_tree);

  int i=2;
  while(i--) {
    for(int d = kMinTreeDepth; d <= kMaxTreeDepth; d += 2) {
      time_construction(d);
    };
  }  

  if (long_lived_tree == 0 || array[1000] != 1.0/1000) cout << "Failed" << endl;
}

#endif

static void pip_populate(State& state, int depth, Pair* node) {
  Pair* cell = 0;
  PIP_FRAME(state, cell, node);
  if(depth <= 0) return;
  depth--;

  cell = state.cons(PIP_FALSE, PIP_FALSE);
  node->car = cell;

  cell = state.cons(PIP_FALSE, PIP_FALSE);
  node->cdr = cell;

  pip_populate(state, depth, (Pair*)node->car);
  pip_populate(state, depth, (Pair*)node->cdr);
}

static Pair* pip_make_tree(State& state, int depth) {
  if(depth <= 0) {
    return state.cons(PIP_FALSE, PIP_FALSE);
  } else {
    Value *kar = 0, *kdr = 0, *cell = 0;
    PIP_FRAME(state, kar, kdr, cell);
    kar = pip_make_tree(state, depth-1);
    kdr = pip_make_tree(state, depth-1);
    cell = (Pair*)state.cons(kar, kdr);
    return (Pair*)cell;
  }
}

static void pip_time_construction(State& state, int depth) {
  long start;
  long bu_time = 0, td_time = 0;
  int iterations = numiters(depth);
  Pair* temp_tree = 0;
  PIP_FRAME(state, temp_tree);
  
  start = CURRENT_TIME();
  for(int i = 0; i < iterations; ++i) {
    temp_tree = state.cons(PIP_FALSE, PIP_FALSE);
    pip_populate(state, depth, temp_tree);
  }
  td_time = CURRENT_TIME() - start;
  start = CURRENT_TIME();
  for(int i = 0; i < iterations; ++i) {
    temp_tree = pip_make_tree(state, depth);
  }
  bu_time = CURRENT_TIME() - start;
  cout << "> Created " << iterations << " trees of depth " << depth;
  cout << " (top down: " << td_time << "ms" << " bottom up: " << bu_time << "ms)" << endl;
}

static void bench_pip() {
  State state;

  Pair *root = 0, *long_lived_tree = 0, *temp_tree = 0;
  Blob* array = 0;
  PIP_FRAME(state, root, long_lived_tree, temp_tree, array);

  temp_tree = pip_make_tree(state, kStretchTreeDepth);
  temp_tree = 0;

  // Create long-lived array
  cout << "! Creating a long-lived array of " << kArraySize << " doubles " << endl;
  array = state.make_blob(sizeof(double) * kArraySize);

  for(int i = 0; i < kArraySize / 2; ++i) {
    ((double*)array->data)[i] = 1.0 / i;
  }

  cout << "! Creating a long-lived binary tree of depth " << kLongLivedTreeDepth << endl;

  // Create long-lived tree
  long_lived_tree = state.cons(PIP_FALSE, PIP_FALSE);
  pip_populate(state, kLongLivedTreeDepth, long_lived_tree);

  int i=2;
  while(i--) {
    for(int d = kMinTreeDepth; d <= kMaxTreeDepth; d += 2) {
      pip_time_construction(state, d);
    };
  }  
 
  if (long_lived_tree == 0 || ((double*)array->data)[1000] != 1.0/1000)
    cout << "Failed" << endl;

  cout << "! Completed " << state.collections << " collections " << endl;
  cout << "! Heap size is " << FriendlySize(state.heap_size) << endl;
}

int main(int argc, char** argv) {
  long start = CURRENT_TIME();
  cout << "! BEGIN " << (BOEHM_GC ? "BOEHM" : "PIP") << endl;
  cout << "! Live storage will peak at " << 
    FriendlySize((2 * sizeof(Pair) * tree_size(kLongLivedTreeDepth)) + (sizeof(double) * kArraySize)) << endl <<
    "! Stretching memory with a binary tree of depth " << kStretchTreeDepth << endl <<
    "! Long-lived binary tree of depth " << kLongLivedTreeDepth << " and long-lived array of " << kArraySize << " doubles " << endl;
  cout.flush();

#if BOEHM_GC
  GC_init();
  bench_boehm(); 
  cout << "! Completed " << GC_gc_no << " collections" << endl;
  cout << "! Heap size is " << FriendlySize(GC_get_heap_size()) << endl;
#else
  bench_pip();
#endif

  cout << "! Completed in " << CURRENT_TIME() - start << "ms" << endl;
}
