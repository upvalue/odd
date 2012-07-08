// pip.hpp - r7rs scheme in one header

// Copyright (c) 2011-2012 ioddly
// Website: <http://ioddly.com/projects/pip/>
// Released under the Boost Software License:
// <http://www.boost.org/LICENSE_1_0.txt>

#ifndef PIP_HPP
#define PIP_HPP

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <iostream>
#include <vector>

#ifndef PIP_DEBUG
# define PIP_ASSERT(x) (assert((x)))
#else
# define PIP_ASSERT(x) ((void)0)
#endif

#define PIP_GC_MSG(x) std::cout << x << std::endl

#ifndef PIP_HEAP_ALIGNMENT
# define PIP_HEAP_ALIGNMENT 4096
#endif

#ifndef PIP_GROW_RATIO
# define PIP_GROW_RATIO 77
#endif

namespace pip {

struct State;
struct Value;

std::ostream& operator<<(std::ostream& os, pip::Value* x);

///// UTILITIES

// Align size along boundary (eg align(4,3) => 4, align(4,5) => 8)
inline size_t align(size_t boundary, size_t size) {
  return ((((size - 1) / (boundary)) + 1) * boundary);
}

// Print a byte count in a readable format (eg 4096 => 4K etc)
struct FriendlySize {
  size_t s;
  FriendlySize(size_t s_): s(s_) {}
};

inline std::ostream& operator<<(std::ostream& os, FriendlySize f) {
  char ending = 'b';
  if(f.s >= 1024) { f.s /= 1024; ending = 'K'; }
  if(f.s >= 1024) { f.s /= 1024; ending = 'M'; }
  return os << f.s << ending;
}

///// OBJECT REPRESENTATION

#define PIP_CAST(klass, x) ((klass *) x)
//# define PIP_CAST(klass, x) (PIP_ASSERT((x)->get_type() == pip::##klass ::CLASS_TYPE), (klass *) (x))

#define PIP_FALSE ((pip::Value*) 0)
#define PIP_TRUE ((pip::Value*) 2)
#define PIP_NULL ((pip::Value*) 6)
#define PIP_EOF ((pip::Value*) 10)

struct SourceInfo { unsigned file, line; };

enum Type {
  TRANSIENT,
  FIXNUM,
  CONSTANT,
  PAIR,
  ARRAY,
  BLOB,
  SYMBOL
};

struct Value {
  size_t size;
  unsigned header;

  ptrdiff_t bits() const { return (ptrdiff_t) this; }
  bool fixnump() const { return bits() & 1; }
  bool constantp() const { return !this || (bits() & 3) == 2; }
  bool pointerp() const { return this && (bits() & 3) == 0; }
  bool immediatep() const { return !pointerp(); }

  int get_header_bit(Type type, int bit) const { PIP_ASSERT(pointerp()); PIP_ASSERT(get_type() == type); return header & bit; }
  void set_header_bit(Type type, int bit) { PIP_ASSERT(get_type() == type); header += bit; }

  void set_header_unsafe(unsigned char type, bool mark, size_t mark_bit) {
    header = (mark ? (mark_bit << 8) : (!mark_bit << 8)) + type;
  }

  void set_size(size_t size_) { size = size_; }

  unsigned char get_type_unsafe() const { return header & 255; };

  Type get_type() const {
    return fixnump() ? FIXNUM : (constantp() ? CONSTANT : static_cast<Type>(get_type_unsafe()));
  }

  int get_mark_unsafe() const { return (header & (1 << 8)) != 0; }
  size_t get_size_unsafe() const { return size; }
  void set_size_unsafe(size_t size_) { size = size_; }
  void flip_mark_unsafe() { header += get_mark_unsafe() ? -256 : 256; }
  void set_type_unsafe(int type) { header = (get_mark_unsafe() << 8) + type; }

  // Fixnums

  static Value* make_fixnum(ptrdiff_t n) { return (Value*) ((n << 1) + 1); }
  ptrdiff_t fixnum_value() const { return bits() >> 1; }

  // Pairs
  static const int PAIR_HAS_SOURCE_BIT = 1 << 15;
  static const int PAIR_TRUE_PAIR_BIT = 1 << 14;

  bool pair_has_source() const { return get_header_bit(PAIR, PAIR_HAS_SOURCE_BIT); }
  bool pair_true_pair() const { return get_header_bit(PAIR, PAIR_TRUE_PAIR_BIT); }

  Value* car() const;
  Value* cdr() const;

  // Arrays
  Value** array_data();
  Value* array_ref(size_t i);
  size_t array_length() const;

  // Blobs
  static const int BLOB_STRING_BIT = 1 << 15;
  
  bool blob_string_bit() const { return get_header_bit(BLOB, BLOB_STRING_BIT); }

  // Symbols
  Value* symbol_name() const;
  Value* symbol_value() const;
};

struct Pair : Value {
  Value *car, *cdr;
  SourceInfo src;
  
  static const Type CLASS_TYPE = PAIR;
};

Value* Value::car() const { PIP_ASSERT(get_type() == PAIR); return static_cast<const Pair*>(this)->car; }
Value* Value::cdr() const { PIP_ASSERT(get_type() == PAIR); return static_cast<const Pair*>(this)->cdr; }

struct Array : Value {
  unsigned length;
  Value* data[1];
};

Value** Value::array_data() { PIP_ASSERT(get_type() == ARRAY); return static_cast<Array*>(this)->data; }
Value* Value::array_ref(size_t i) { PIP_ASSERT(get_type() == ARRAY); return array_data()[i]; }
size_t Value::array_length() const { PIP_ASSERT(get_type() == ARRAY); return static_cast<const Array*>(this)->length; }

struct Blob : Value {
  unsigned length;
  char data[1];

  static const Type CLASS_TYPE = BLOB;
};

struct Symbol : Value {
  Value *name, *value;
};

Value* Value::symbol_name() const { PIP_ASSERT(get_type() == SYMBOL); return static_cast<const Symbol*>(this)->name; }
Value* Value::symbol_value() const { PIP_ASSERT(get_type() == SYMBOL); return static_cast<const Symbol*>(this)->value; }

///// GARBAGE COLLECTOR TRACKING

// This structure is used in the GC tracking macros to take a normal
// pointer to a Pip value of any type and obtain a Value** pointer to it
struct FrameHack {
#define PIP_(t) FrameHack(t *& ref): ref((Value**) &ref) {}
  PIP_(Value)
  PIP_(Pair)
  PIP_(Blob)
  PIP_(Symbol)
  PIP_(Array)
#undef PIP_
  
  ~FrameHack() {}
  Value** ref;
};

// Frames are are stack-allocated arrays of variables that push and
// pop themselves along with the program stack.
struct Frame {
  Frame(State&, FrameHack*, size_t);
  ~Frame();

  State& state;
  Value*** roots;
  size_t root_count;
  Frame* previous;
};

#define PIP_FRAME(state, ...) \
  pip::FrameHack __pip_frame_hacks[] = { __VA_ARGS__ }; \
  pip::Frame __pip_frame((state), (FrameHack*) __pip_frame_hacks, sizeof(__pip_frame_hacks) / sizeof(pip::FrameHack))

#define PIP_S_FRAME(...) PIP_FRAME((*this), __VA_ARGS__)

struct State {

  struct Block {
    size_t size;
    char *begin, *end;
    
    Block(size_t size, bool mark, int mark_bit) {
      begin = (char*) malloc(size);
      end = begin + size;
      ((Value*) begin)->set_header_unsafe(TRANSIENT, mark, mark_bit);
      ((Value*) begin)->set_size(size);
    }

    ~Block() {
      free(begin);
    }
  };

  State(): 

    frame_list(0), 
    collect_before_every_allocation(false), heap_size(PIP_HEAP_ALIGNMENT),
    mark_bit(1), block_cursor(0), sweep_cursor(0), live_at_last_collection(0),
    collections(0) {

    Block* first = new Block(PIP_HEAP_ALIGNMENT, false, mark_bit);
    blocks.push_back(first);
    sweep_cursor = first->begin;
   
  }

  ~State() {
    for(size_t i = 0; i != blocks.size(); i++) {
      delete blocks[i];
    }
  }

  ///// GARBAGE COLLECTOR

  static const size_t GC_TEMPS_SIZE = 10;

  std::vector<Block*> blocks;
  Frame* frame_list;
  bool collect_before_every_allocation;
  size_t heap_size, mark_bit, block_cursor, live_at_last_collection, collections;
  char* sweep_cursor;

  bool markedp(Value* x) {
    return x->get_mark_unsafe() == mark_bit;
  }

  void recursive_mark(Value* x) {
    while(x->pointerp() && !markedp(x)) {
      live_at_last_collection += x->get_size_unsafe();
      x->flip_mark_unsafe();

      switch(x->get_type_unsafe()) {
        case PAIR:
          recursive_mark(x->car());
          x = x->cdr();
          continue;
        case ARRAY:
          for(size_t i = 0; i != x->array_length(); i++) {
            recursive_mark(x->array_ref(i));
          }
          x = x->array_ref(x->array_length() - 1);
          continue;
        case SYMBOL:
          continue;
        case BLOB:
          return;
        default: PIP_ASSERT(!"recursive_mark bad;"); return;
      }
    }
  }

  // This really is the meat of the garbage collector. It's just a
  // first fit allocator that makes sure things are appropriately marked.
  char* findfree(size_t required) {
    char* address = NULL;
    Value* sweep_cursor_v = NULL, *address_v = NULL;
    Block* block = NULL;

    while(block_cursor != blocks.size()) {

      block = blocks[block_cursor];

      PIP_ASSERT(sweep_cursor >= block->begin && sweep_cursor <= block->end);
      while(sweep_cursor != block->end) {
        sweep_cursor_v = (Value*) sweep_cursor;
        // If this space is used, skip it
        if(markedp(sweep_cursor_v)) {
          sweep_cursor += sweep_cursor_v->get_size_unsafe();
          sweep_cursor_v = (Value*) sweep_cursor;
          // Should never go past the end of the heap
          PIP_ASSERT(sweep_cursor <= block->end);
          continue;
        }

        // Free space, figure out how large it is
        size_t hole_size = sweep_cursor_v->get_size_unsafe();
        // Should never have a hole that can't fit object prefix
        PIP_ASSERT(hole_size >= sizeof(Value));
      
        // If the hole is not big enough, see if we can coalesce holes
        // in front of it
        while(hole_size < required) {
          Value* next = (Value*) (sweep_cursor + hole_size);

          if(next == (Value*) block->end) {
            break;
          }
          PIP_ASSERT((char*)next <= block->end);

          if(!markedp(next)) {
            hole_size += next->get_size_unsafe();
            sweep_cursor_v->set_size_unsafe(hole_size);
          } else {
            // End of the hole
            break;
          }
        }

        // If the hole is now large enough
        if(hole_size >= required) {
          // Take what we need from the front
          address = sweep_cursor;
          sweep_cursor += required;
          sweep_cursor_v = (Value*) sweep_cursor;
          // If the new hole is not large enough to hold an object, we
          // will just append it to the end of this object
          size_t new_hole_size = hole_size - required;
          if(new_hole_size < sizeof(Value)+sizeof(size_t)) {
            sweep_cursor += new_hole_size;
            sweep_cursor_v = (Value*) sweep_cursor;
          } else {
            // If it is large enough, update size info
            sweep_cursor_v->set_size_unsafe(new_hole_size);
            sweep_cursor_v->set_header_unsafe(TRANSIENT, false, mark_bit);
            PIP_ASSERT(!markedp(sweep_cursor_v));
          }

          // Determine how much memory we actually allocated
          size_t actual_size = (size_t) (sweep_cursor - address);
          address_v = (Value*) address;
          memset(address, 0, actual_size);
          address_v->set_size_unsafe(actual_size);
          // Ensure object is marked (since we cleared it, it may or may
          // not be considered marked based on mark_bit)
          if(!markedp(address_v))
            address_v->flip_mark_unsafe();

          PIP_ASSERT(address_v->get_size_unsafe() == actual_size);
          PIP_ASSERT(markedp(address_v));

          // Done!
          break;
        }

        // The hole is not big enough, mark as used and move on
        sweep_cursor_v->flip_mark_unsafe();
        PIP_ASSERT(markedp(sweep_cursor_v));
      
        sweep_cursor += hole_size;
        sweep_cursor_v = (Value*) sweep_cursor;
      }

      // break out of inner loop
      if(address) break;

      block_cursor++;

      if(block_cursor != blocks.size())
        sweep_cursor = blocks[block_cursor]->begin;
    }

    // Addresses returned should always be marked as used
    if(address)
      PIP_ASSERT(markedp(address_v));
    return address;
  }

  void compact() {
    // Mark all live memory

    // Create a new block to copy the heap to
    Block* heap = new Block(align(PIP_HEAP_ALIGNMENT, heap_size), false, mark_bit);

    // Loop over live memory, allocating from the new block. The new
    // allocation is called the "forwarding pointer" and the object's
    // 'size' field is used to store the forwarding pointer (the newly
    // allocated space stores the object's size)

    // Loop over the heap and copy all objects, updating pointers as
    // you go.
  }

  void collect(size_t request = 0, bool force_request = false) {
      collections++;

    // If collection is called early, we have to sweep over everything
    // and make sure it's marked as used

    while(block_cursor != blocks.size()) {
      Block* b = blocks[block_cursor];
      PIP_ASSERT(sweep_cursor >= b->begin && sweep_cursor <= b->end);
      while(sweep_cursor != b->end) {
        Value* sweep_cursor_v = (Value*) sweep_cursor;
        if(!markedp(sweep_cursor_v)) {
          sweep_cursor_v->flip_mark_unsafe();
          PIP_ASSERT(markedp(sweep_cursor_v));
        }
        sweep_cursor += sweep_cursor_v->get_size_unsafe();
        PIP_ASSERT(sweep_cursor <= b->end);
      }
      block_cursor++;
      if(block_cursor != blocks.size())
        sweep_cursor = blocks[block_cursor]->begin;
    }

    // Reset sweep cursor
    sweep_cursor = blocks[0]->begin;
    block_cursor = 0;

    // If growth is necessary

    // Determine whether we should grow
    // This will happen if
    // a) Most of the heap is in use
    // b) A collection has failed to free up enough space for an allocation

    size_t pressure = (live_at_last_collection * 100) / heap_size;
    if(pressure >= PIP_GROW_RATIO || force_request) {
      // We're gonna grow
      size_t new_block_size = (heap_size);
    
      PIP_ASSERT(new_block_size >= request);

      // Make absolutely sure we can accomodate that request
      if(new_block_size < request)
        new_block_size = align(PIP_HEAP_ALIGNMENT, (heap_size) + request);

      Block* b = new Block(new_block_size, true, mark_bit);
      blocks.push_back(b);

      //PIP_ASSERT(!markedp((Value*)b->begin));

      heap_size += new_block_size;

      PIP_GC_MSG("growing heap from " << FriendlySize(heap_size - new_block_size) << " to " << FriendlySize(heap_size)
                 << " because of " << (pressure >= PIP_GROW_RATIO ? "pressure" : "allocation failure"));
    }

    live_at_last_collection = 0;

    // This is the beauty of the mark-and-don't-sweep algorithm. All
    // the work has been done by the allocator, so now all we have to
    // do is flip the mark bit so all memory is considered dead. Then
    // we immediately mark live memory, and collection is done. No
    // sweep required.
    
    // Flip mark bit so now all dead memory is known
    mark_bit = !mark_bit;

    // Mark all live memory
    for(Frame* f = frame_list; f != NULL; f = f->previous)
      for(size_t i = 0; i != f->root_count; i++)
        recursive_mark(*f->roots[i]);
  }

  Value* allocate(Type type, size_t size) {
    if(collect_before_every_allocation) {
      collect();
    }
  
    // This function calls findfree and does some minor tweaks before
    // and after

    // Align along pointer-sized boundary to ensure that our immediate
    // value scheme will work.
    size = align(sizeof(void*), size);

    if(size >= PIP_HEAP_ALIGNMENT) {
      size = align(PIP_HEAP_ALIGNMENT, size);
      Block* block = new Block(size, false, mark_bit);
      blocks.push_back(block);
      Value* x = (Value*) block->begin;
      x->set_type_unsafe(type);
      x->flip_mark_unsafe();
      PIP_ASSERT(markedp(x));

      heap_size += size;

      PIP_GC_MSG("growing heap from " << FriendlySize(heap_size - size) << " to " << FriendlySize(heap_size)
                 << " because of large object allocation");

      return x;
    }

    // Try finding memory
    char* address = findfree(size);

    if(!address) {
      collect(size);
      address = findfree(size);
      if(!address) {
        collect(size, true);
        address = findfree(size);
        if(!address) {
          std::cerr << "could not allocate " << size << " bytes " << std::endl;

          PIP_ASSERT(!"out of memory");
        }
      }
    }

    Value* x = (Value*) address;

    x->set_type_unsafe(type);

    PIP_ASSERT(x->get_size_unsafe() >= size);
    PIP_ASSERT(markedp(x));
    PIP_ASSERT(x->get_type_unsafe() == type);
    PIP_ASSERT(x->pointerp());

    return x;
  }
  
  template <class T> T* allocate(ptrdiff_t additional = 0) {
    return static_cast<T*>(allocate(static_cast<Type>(T::CLASS_TYPE), sizeof(T) + additional));
  }

  ///// CONSTRUCTORS
  Pair* cons(Value* car, Value* cdr, bool has_source_info = false, unsigned file = 0, unsigned line = 0) {
    Pair* p = NULL;
    PIP_S_FRAME(p, car, cdr);
    p = allocate<Pair>(has_source_info ? 0 : -(sizeof(((Pair*)0)->src)));
    p->car = car;
    p->cdr = cdr;
    if(has_source_info) {
      p->set_header_bit(PAIR, Value::PAIR_HAS_SOURCE_BIT);
      p->src.file = file;
      p->src.line = line;
    }
    return p;
  }

  Blob* make_blob(ptrdiff_t length) {
    Blob* b = allocate<Blob>(length - 1);
    b->length = length;
    return b;
  }

  ///// FUNCTIONS

  ///// READER
  void prime_reader() {

  }
  
  ///// VIRTUAL MACHINE
}; // State

///// GARBAGE COLLECTOR TRACKING IMPLEMENTATION

inline Frame::Frame(State& state_, FrameHack* roots_, size_t root_count_):
  state(state_),
  roots((Value***)roots_),
  root_count(root_count_), previous(NULL) {

  if(state.frame_list)
    previous = state.frame_list;
  state.frame_list = this;
}

inline Frame::~Frame() {
  PIP_ASSERT("odd::Frame FIFO order violated" && state.frame_list == this);
  state.frame_list = previous;
}


inline std::ostream& operator<<(std::ostream& os, pip::Value* x) {
  switch(x->get_type()) {
    case pip::CONSTANT:
      if(x == PIP_TRUE) return os << "#t";
      if(x == PIP_FALSE) return os << "#f";
      if(x == PIP_EOF) return os << "#<eof>";
      if(x == PIP_NULL) return os << "()";
      break;
    case pip::SYMBOL:
      return os;//return os << x->symbol_name()->string_data();
    case pip::FIXNUM:
      return os << x->fixnum_value();
    case pip::PAIR: {
      os << '(' << x->car();
      for(x = x->cdr(); x->get_type() == pip::PAIR; x = x->cdr()) {
        os << ' ' << x->car();
      }
      // Dotted list
      if(x != PIP_NULL) {
        os << " . " << x;
      }
      return os << ')';
    }  
    default:
      return os << "#<unknown value " << (size_t) x << " type: " << x->get_type() << ">";
  }
  return os;
}

} // pip

using pip::operator<<;

#endif
