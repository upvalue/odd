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

#include <limits>
#include <fstream>
#include <sstream>
#include <iostream>
#include <list>
#include <vector>
#include <unordered_map> // TODO: Alternatives to c++0x hash tables

#ifndef PIP_DEBUG
# define PIP_ASSERT(x) (assert((x)))
#else
# define PIP_ASSERT(x) ((void)0)
#endif

#define PIP_CHECK(x) if((x)->active_exception()) return (x);
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
struct String;
struct Symbol;

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

#define PIP_CAST(klass, x) (PIP_ASSERT((x)->get_type() == pip::klass ::CLASS_TYPE), (klass *) (x))

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
  VECTOR,
  BLOB,
  STRING,
  SYMBOL,
  EXCEPTION,
  BOX
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

  void set_car(Value*);
  void set_cdr(Value*);

  // Vectors
  Value** vector_data();
  Value* vector_ref(size_t i);
  size_t vector_length() const;

  // Blobs
  const char* blob_data() const;
  unsigned blob_length() const;

  // Strings
  const char* string_data() const;
  unsigned string_length() const;

  // Symbols
  Value* symbol_name() const;
  Value* symbol_value() const;

  // Exceptions
  static const int EXCEPTION_ACTIVE_BIT = 1 << 15;
  
  Symbol* exception_tag() const;
  String* exception_message() const;
  Value* exception_irritants() const;
  
  bool active_exception() const { return get_type() == EXCEPTION && (header & EXCEPTION_ACTIVE_BIT); }

  // Boxes
  static const int BOX_HAS_SOURCE_BIT = 1 << 15;

  bool box_has_source() const { return get_header_bit(BOX, BOX_HAS_SOURCE_BIT); }

  Value* box_value() const;
};

struct Pair : Value {
  Value *car, *cdr;
  SourceInfo src;
  
  static const Type CLASS_TYPE = PAIR;
};

Value* Value::car() const { PIP_ASSERT(get_type() == PAIR); return static_cast<const Pair*>(this)->car; }
Value* Value::cdr() const { PIP_ASSERT(get_type() == PAIR); return static_cast<const Pair*>(this)->cdr; }

void Value::set_car(Value* car_)  { PIP_ASSERT(get_type() == PAIR); static_cast<Pair*>(this)->car = car_; }
void Value::set_cdr(Value* cdr_)  { PIP_ASSERT(get_type() == PAIR); static_cast<Pair*>(this)->cdr = cdr_; }

struct Vector : Value {
  unsigned length;
  Value* data[1];
};

Value** Value::vector_data() { PIP_ASSERT(get_type() == VECTOR); return static_cast<Vector*>(this)->data; }
Value* Value::vector_ref(size_t i) { PIP_ASSERT(get_type() == VECTOR); return vector_data()[i]; }
size_t Value::vector_length() const { PIP_ASSERT(get_type() == VECTOR); return static_cast<const Vector*>(this)->length; }

struct Blob : Value {
  unsigned length;
  char data[1];

  static const Type CLASS_TYPE = BLOB;
};


const char* Value::blob_data() const { PIP_ASSERT(get_type() == BLOB); return static_cast<const Blob*>(this)->data; }
unsigned Value::blob_length() const { PIP_ASSERT(get_type() == BLOB); return static_cast<const Blob*>(this)->length; }

struct String : Blob {
  static const Type CLASS_TYPE = STRING;
};

const char* Value::string_data() const { PIP_ASSERT(get_type() == STRING); return static_cast<const String*>(this)->data; }
unsigned Value::string_length() const { PIP_ASSERT(get_type() == STRING); return static_cast<const String*>(this)->length; }

struct Symbol : Value {
  Value *name, *value;

  static const Type CLASS_TYPE = SYMBOL;
};

Value* Value::symbol_name() const { PIP_ASSERT(get_type() == SYMBOL); return static_cast<const Symbol*>(this)->name; }
Value* Value::symbol_value() const { PIP_ASSERT(get_type() == SYMBOL); return static_cast<const Symbol*>(this)->value; }

struct Exception : Value {
  Symbol* tag;
  String* message;
  Value* irritants;
  
  static const Type CLASS_TYPE = EXCEPTION;
};

Symbol* Value::exception_tag() const { PIP_ASSERT(get_type() == EXCEPTION); return static_cast<const Exception*>(this)->tag; };
String* Value::exception_message() const { PIP_ASSERT(get_type() == EXCEPTION); return static_cast<const Exception*>(this)->message; };
Value* Value::exception_irritants() const { PIP_ASSERT(get_type() == EXCEPTION); return static_cast<const Exception*>(this)->irritants; };

struct Box : Value {
  Value* value;
  SourceInfo src;

  static const Type CLASS_TYPE = BOX;
};

Value* Value::box_value() const { PIP_ASSERT(get_type() == BOX); return static_cast<const Box*>(this)->value; };
///// GARBAGE COLLECTOR TRACKING

// This structure is used in the GC tracking macros to take a normal
// pointer to a Pip value of any type and obtain a Value** pointer to it
struct FrameHack {
#define PIP_(t) FrameHack(t *& ref): ref((Value**) &ref) {}
  PIP_(Value)
  PIP_(Pair)
  PIP_(Blob)
  PIP_(Symbol)
  PIP_(Vector)
  PIP_(String)
  PIP_(Exception)
  PIP_(Box)
#undef PIP_
  
  ~FrameHack() {}
  Value** ref;
};

// Frames are are stack-allocated vectors of variables that push and
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

// Handles are heap-allocated pointers
template <class T>
struct Handle {
  Handle(State& state_);
  Handle(State& state_, T* ref_);
  ~Handle();

  State& state;
  T* ref;
  std::list<Handle<Value>*>::iterator location;
  
  void initialize();
  T* operator*() const { return ref; }
  T* operator->() const { return ref; }
  void operator=(T* ref_) { ref = ref_; }  
};

typedef std::list<Handle<Value> *> handle_list_t;

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

struct State {
  State(): 

    frame_list(0), 
    collect_before_every_allocation(false), heap_size(PIP_HEAP_ALIGNMENT),
    mark_bit(1), block_cursor(0), sweep_cursor(0), live_at_last_collection(0),
    collections(0),

    source_counter(1) {

    // Initialize garbage collector
    Block* first = new Block(PIP_HEAP_ALIGNMENT, false, mark_bit);
    blocks.push_back(first);
    sweep_cursor = first->begin;

    // Initialize globals
    static const char* global_symbols[] = {
      // Exception tags
      "#pip#read",
      // Special forms
      "define",
      "lambda",
      "quote",
      "quasiquote",
      "unquote",
      "unquote-splicing"
    };

    for(size_t i = 0; i != GLOBAL_SYMBOL_COUNT; i++) {
      Value* s = make_symbol(global_symbols[i]);
      Handle<Value>* v = new Handle<Value>(*this, s);
      globals.push_back(v);
    }

    // Initialize reader
    source_names.push_back("unknown");
    source_contents.push_back("");
  }

  ~State() {
    for(size_t i = 0; i != globals.size(); i++)
      delete globals[i]; 

    for(size_t i = 0; i != blocks.size(); i++)
      delete blocks[i];
  }

  ///// GARBAGE COLLECTOR

  static const size_t GC_TEMPS_SIZE = 10;

  std::vector<Block*> blocks;
  handle_list_t handle_list;
  Frame* frame_list;
  bool collect_before_every_allocation;
  size_t heap_size, mark_bit, block_cursor, live_at_last_collection, collections;
  char* sweep_cursor;

  bool markedp(Value* x) {
    return x->get_mark_unsafe() == mark_bit;
  }

  void recursive_mark(Value* x) {
    // Done in a while loop to save stack space
    while(x->pointerp() && !markedp(x)) {
      live_at_last_collection += x->get_size_unsafe();
      x->flip_mark_unsafe();

      switch(x->get_type_unsafe()) {
        // Atomic
        case STRING: case BLOB:
          return;
        // One pointer
        case BOX:
          x = static_cast<Box*>(x)->value;
          continue;
        // Two pointers 
        case PAIR:
        case SYMBOL:
          recursive_mark(static_cast<Pair*>(x)->car);
          x = static_cast<Pair*>(x)->cdr;
          continue;
        // Three pointers
        case EXCEPTION:
          recursive_mark(static_cast<Exception*>(x)->tag);
          recursive_mark(static_cast<Exception*>(x)->message);
          x = static_cast<Exception*>(x)->irritants;
          continue;
        case VECTOR:
          for(size_t i = 0; i != x->vector_length(); i++) {
            recursive_mark(x->vector_ref(i));
          }
          x = x->vector_ref(x->vector_length() - 1);
          continue;
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

    // Symbols are roots (for now)
    for(symbol_table_t::iterator i = symbol_table.begin(); i != symbol_table.end(); i++)
      recursive_mark(i->second);

    for(Frame* f = frame_list; f != NULL; f = f->previous)
      for(size_t i = 0; i != f->root_count; i++)
        recursive_mark(*f->roots[i]);

    for(handle_list_t::iterator i = handle_list.begin(); i != handle_list.end(); i++)
      recursive_mark((*i)->ref);
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

  ///// RUNTIME
  typedef std::unordered_map<std::string, Symbol*> symbol_table_t;

  std::unordered_map<std::string, Symbol*> symbol_table;
  std::vector<Handle<Value> *> globals;

  enum Global {
    S_PIP_READ,
    S_DEFINE,
    S_LAMBDA,
    S_QUOTE,
    S_QUASIQUOTE,
    S_UNQUOTE,
    S_UNQUOTE_SPLICING,
    GLOBAL_SYMBOL_COUNT
  };

  Symbol* global_symbol(Global g) {
    return static_cast<Symbol*>(**(globals[(size_t) g]));
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

  String* make_string(const std::string& cstr) {
    String* str = allocate<String>(cstr.length());
    cstr.copy(str->data, cstr.length());
    str->data[cstr.length()] = 0;
    return str;
  }

  Symbol* make_symbol(const std::string& str) {
    Symbol* sym = 0;
    symbol_table_t::const_iterator g = symbol_table.find(str);
    // Intern symbol if it hasn't already been
    if(g == symbol_table.end()) {
      String* ostr = 0;
      PIP_S_FRAME(sym, ostr);
      sym = allocate<Symbol>();
      ostr = make_string(str.c_str());
      sym->name = ostr;
      std::pair<std::string, Symbol*> pair(str,sym);
      symbol_table.insert(pair);
    } else {
      sym = g->second;
    }
    return sym;
  }

  Symbol* make_symbol(String* string) {
    std::string str(string->string_data());
    return make_symbol(str);
  }

  Box* make_box(Value* value, bool has_source_info, unsigned file = 0, unsigned line = 0) {
    Box* box = 0;
    PIP_S_FRAME(value, box);
    box = allocate<Box>(has_source_info ? 0 : -(sizeof(((Box*)0)->src)));
    box->value = value;
    if(has_source_info) {
      box->set_header_bit(BOX, Value::BOX_HAS_SOURCE_BIT);
      box->src.file = file;
      box->src.line = line;
    }
    return box;
  }

  Exception* make_exception(Symbol* tag, String* message, Value* irritants) {
    Exception* e = 0;
    PIP_S_FRAME(e,tag,message,irritants);
    e = allocate<Exception>();
    e->tag = tag;
    e->message = message;
    e->irritants = irritants;
    e->set_header_bit(EXCEPTION, Value::EXCEPTION_ACTIVE_BIT);
    return e;
  }

  Exception* make_exception(Global g, const std::string& msg) {
    String* smsg = make_string(msg);
    return make_exception(global_symbol(g), smsg, PIP_FALSE);
  }

  ///// BASIC FUNCTIONS

  void append_m(Value** head, Value** tail, Value* value) {
    Value* h = *head, *t = *tail, *swap = NULL;
    PIP_S_FRAME(h, t, swap, value);
    swap = cons(value, PIP_NULL);
    if(!h) {
      (*head) = swap;
      (*tail) = swap;
    } else {
      t->set_cdr(swap);
      (*tail) = swap;
    }
  }

  ///// READER
  std::vector<std::string> source_names;
  std::vector<std::string> source_contents;
  unsigned source_counter;

  // Load the source code for a file (for debugging purposes)
  unsigned register_file(const char* path) {
    size_t length;
    std::fstream fs(path, std::fstream::in);
    // TODO: Check validity
    fs.seekg(0, std::ios::end);
    length = fs.tellg();
    fs.seekg(0, std::ios::beg);
    char* buffer = new char[length+1];
    fs.read(buffer, length);
    buffer[length] = 0;
    source_names.push_back(path);
    source_contents.push_back(buffer);
    fs.close();
    delete[] buffer;
    return source_counter++;
  }

  struct Reader {
    // TODO: Replace with a table of properties to avoid branching
    static bool quotep(char c) {
      return c == '\'' || c == '`' || c == ',';
    }
    
    static bool delimiterp(char c) {
      return c == '(' || c == ')' || c == ';' || c == '@' ||
        c == EOF || isspace(c) || c == '.' || c == '#' || c == '"' || quotep(c);
    }

    static bool symbol_startp(char c) {
      return (isalpha(c) || ispunct(c)) && !delimiterp(c);
    }

    static bool symbolp(char c) { 
      return symbol_startp(c) || (!delimiterp(c) && isdigit(c));
    }

    enum Token {
      TK_LOOKAHEAD,
      TK_EOF,
      TK_FIXNUM,
      TK_WHITESPACE,
      TK_NEWLINE,
      TK_LPAREN,
      TK_RPAREN,
      TK_SYMBOL,
      TK_STRING,
      TK_TRUE,
      TK_FALSE,
      TK_EXCEPTION,
      TK_DOT,
      TK_QUOTE,
      TK_QUASIQUOTE,
      TK_UNQUOTE,
      TK_UNQUOTE_SPLICING
    };

    Reader(State& state_, std::istream& input_, unsigned file_): state(state_), input(input_),
    file(file_), line(0), token_value(state), token_lookahead(TK_LOOKAHEAD) {
      
    }
    ~Reader() { }

    State& state;
    std::istream& input;
    unsigned file, line;
    Handle<Value> token_value;
    Token token_lookahead;
    std::string token_buffer;

    // Tokenizer
    char getc() {
      char c = (char) input.get();
      if(c == '\n') line++;
      return c;
    }

    void ungetc(char c) {
      if(c == '\n') line--;
      input.putback(c);
    }

    Token lex_error(const std::string& msg) {
      std::stringstream ss;
      ss << state.source_names[file] << ' ' << line << ": " << msg;
      token_value = state.make_exception(State::S_PIP_READ, ss.str());
      return TK_EXCEPTION;
    }

    Value* parse_error(const std::string& msg) {
      std::stringstream ss;
      ss << state.source_names[file] << ' ' << line<< ": " << msg;
      return state.make_exception(State::S_PIP_READ, ss.str());
    }

    Token next_token(bool eat_whitespace = true, bool eat_newlines = false) {
      // Return a lookahead token, if necessary
      if(token_lookahead != TK_LOOKAHEAD) {
        Token t = token_lookahead;
        token_lookahead = TK_LOOKAHEAD;
        if(!eat_newlines || (eat_newlines && t != TK_NEWLINE))
          if(!eat_whitespace || (eat_whitespace && t != TK_WHITESPACE))
            return t;
      }

      char c;
      while((c = getc())) {
        // Numbers
        if(isdigit(c)) {
          ptrdiff_t n = 0, digits = 0;
          do {
            n = (n * 10) + (c - '0');
            digits++;
          } while(isdigit(c = getc()));
          ungetc(c);

          if(digits >= std::numeric_limits<ptrdiff_t>::digits10 - 1) {
            std::cerr << "WARNING: integer overflow while reading fixnum" << std::endl;
          }
        
          token_value = Value::make_fixnum(n);
          return TK_FIXNUM;
        } else if(symbol_startp(c)) {
          // Symbols
          token_buffer.clear();

          token_buffer += c;
          while((c = getc())) {
            if(symbolp(c)) {
              token_buffer += c;
            } else {
              ungetc(c);
              
              Symbol* s = 0;
              PIP_FRAME(state, s);
              s = state.make_symbol(token_buffer);
              token_value = state.make_box(s, true, file, line);

              return TK_SYMBOL;
            }
          }
        }

        switch(c) {
          // Eat newlines
          case '\n':
            if(eat_newlines) continue;
            return TK_NEWLINE;
          case '\r': case ' ': case '\t':
            if(eat_whitespace) continue;
            return TK_WHITESPACE;
          case '.': return TK_DOT;
          case '(': return TK_LPAREN;
          case ')': return TK_RPAREN;
          case '\'': return TK_QUOTE;
          case '`': return TK_QUASIQUOTE;
          case ',': {
            c = getc();
            if(c == '@') return TK_UNQUOTE_SPLICING;
            ungetc(c);
            return TK_UNQUOTE;
          }
          case '"': {
            token_buffer.clear();
            while((c = getc())) {
              if(c == EOF) {
                return lex_error("unexpected end of file in string");
              } else if(c == '"') {
                token_value = state.make_string(token_buffer);
                return TK_STRING;
              } else {
                token_buffer += c;
              }
            }
          }
          case '#': {
            c = getc();
            switch(c) {
              case 't': case 'T': return TK_TRUE;
              case 'f': case 'F': return TK_FALSE;
              case EOF: return lex_error("unexpected EOF after #");
              case '\n': return lex_error("unexpected newline after #");
              default: {
                std::string msg = "unknown read syntax \'";
                msg += c;
                msg += '\'';
                return lex_error(msg);
              }
            }
          }
          case EOF: return TK_EOF;
        }
      }
    }
 
    Value* pop_token_value() {
      Value* value = *token_value;
      token_value = PIP_FALSE;
      return value;
    }

    void discard_lookahead() {
      token_lookahead = TK_LOOKAHEAD;
    }

    Token peek_token(bool eat_whitespace = true, bool eat_newlines = false) {
      Token t = next_token(eat_whitespace, eat_newlines);
      token_lookahead = t;
      return t;
    }
    
#define PIP_READ_CHECK_TK(x) if((x) == TK_EXCEPTION) return pop_token_value(); 

    Value* read_list(Value* initial = 0) {
      Value *head = 0, *tail = 0, *elt = 0, *swap = 0;
      // Save the line this list begins on
      size_t l = line; 
      PIP_FRAME(state, initial, head, tail, elt, swap);
      if(initial != NULL) {
        swap = state.cons(initial, PIP_NULL);
        head = tail = swap;
        swap = NULL;
      }

      Token token;
      // Read elements
      while(1) {
        token = peek_token(true, true);
        PIP_READ_CHECK_TK(token);

        if(token == TK_EOF) {
          std::stringstream ss;
          ss << "unterminated list beginning on line " << line;
          return parse_error(ss.str());
        }

        if(token == TK_RPAREN) {
          discard_lookahead();
          break;
        }

        if(token == TK_DOT) {
          if(!head) {
            return parse_error("dot at beginning of list");
          } else {
            discard_lookahead();
            break;
          }
        }

        // Read a list element
        elt = read();
        PIP_CHECK(elt);

        state.append_m(&head, &tail, elt);
      }

      // Read last element of dotted list
      if(token == TK_DOT) {
        token = peek_token();
        PIP_READ_CHECK_TK(token);
        if(token == TK_EOF) {
          return parse_error("unexpected end of file in dotted list");
        } else if(token == TK_RPAREN) {
          return parse_error("dotted list ended early");
        } else {
          elt = read();
          PIP_CHECK(elt);
          tail->set_cdr(elt);
        }
        token = peek_token();
        PIP_READ_CHECK_TK(token);
        if(token != TK_RPAREN) return parse_error("more than one element at the end of a dotted list");
        discard_lookahead();
      }

      // Attach source info
      if(head->get_type() == PAIR) {
        swap = head->car();
        elt = head->cdr();
        head = state.cons(swap, elt, true, file, l);
      }

      return head == PIP_FALSE ? PIP_NULL : head;
    }

    Value* read() {
      Token token;

      while(true) {
        token = next_token();
        switch(token) {
          // Immediate return
          case TK_EXCEPTION:
          case TK_STRING:
          case TK_FIXNUM:
          case TK_SYMBOL:
            return pop_token_value();
          case TK_LPAREN: {
            if(peek_token(false, false) == TK_RPAREN) return PIP_NULL;
            return read_list();
          }
          case TK_RPAREN: return parse_error("unexpected ')'");
          // Literals
          case TK_TRUE: return PIP_TRUE;
          case TK_FALSE: return PIP_FALSE;
          // Quotes
          case TK_QUOTE:
          case TK_QUASIQUOTE:
          case TK_UNQUOTE:
          case TK_UNQUOTE_SPLICING: {
            State::Global g = State::S_QUOTE;
            switch(token) {
              case TK_QUASIQUOTE: g = State::S_QUASIQUOTE; break;
              case TK_UNQUOTE: g = State::S_UNQUOTE; break;
              case TK_UNQUOTE_SPLICING: g = S_UNQUOTE_SPLICING; break;
              default: break;
            }
            size_t l = line;
            Value *cell = 0, *cell2 = 0, *value = 0, *symbol = 0;
            PIP_FRAME(state, cell, cell2, value, symbol);
            value = read();
            PIP_CHECK(value);
            if(value == PIP_EOF) return parse_error("unexpected EOF after quote");
            symbol = state.global_symbol(g);
            cell2 = state.cons(value, PIP_NULL);
            cell = state.cons(symbol, cell2, true, file, l);
            return cell;
          }
          case TK_EOF: return PIP_EOF;
        }
      }
    }
  };

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

template <class T>
inline Handle<T>::Handle(State& state_): state(state_), ref(0) {
  initialize();
}

template <class T>
inline Handle<T>::Handle(State& state_, T* ref_): state(state_), ref(ref_) {
  initialize();
}

template <class T>
inline void Handle<T>::initialize() {
  location = state.handle_list.insert(state.handle_list.begin(),
      (Handle<Value> *) this);
}

template <class T>
inline Handle<T>::~Handle() {
  state.handle_list.erase(location);
}

inline std::ostream& operator<<(std::ostream& os, Value* x) {
  switch(x->get_type()) {
    case CONSTANT:
      if(x == PIP_TRUE) return os << "#t";
      if(x == PIP_FALSE) return os << "#f";
      if(x == PIP_EOF) return os << "#<eof>";
      if(x == PIP_NULL) return os << "()";
      break;
    case SYMBOL:
      return os << x->symbol_name()->string_data();
    case FIXNUM:
      return os << x->fixnum_value();
    case BLOB:
        return os << "#<blob " << x->blob_length() << '>';
    case STRING:
        return os << x->string_data();
    case PAIR: {
      os << '(' << x->car();
      for(x = x->cdr(); x->get_type() == PAIR; x = x->cdr()) {
        os << ' ' << x->car();
      }
      // Dotted list
      if(x != PIP_NULL) {
        os << " . " << x;
      }
      return os << ')';
    }  
    case BOX: {
      // If it has source info then it's some value wrapped by the reader
      if(x->box_has_source())
        return os << x->box_value();
      else
        return os << "#<box " << (ptrdiff_t)x << ">";
    }
    case EXCEPTION: {
      return os << "#<exception " << x->exception_tag() << ' ' << x->exception_message() << '>';
    }
    default:
      return os << "#<unknown value " << (size_t) x << " type: " << x->get_type() << ">";
  }
  return os;
}

} // pip

using pip::operator<<;

#endif
