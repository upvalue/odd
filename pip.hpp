// pip.hpp - r7rs scheme in (almost) one header

// Copyright (c) 2011-2012 ioddly
// Website: <http://ioddly.com/projects/pip/>
// Released under the Boost Software License:
// <http://www.boost.org/LICENSE_1_0.txt>

// Welcome to Pip Scheme!

// Pip is an implementation of the Scheme programming language. In addition to
// that, it's also something of a guided tour through language implementation.
// You can use the following "table of contents" to navigate the source code.

// 1. (PRELUDE) Preprocessor definitions and miscellaneous utilities
// 2. (OBJ) Object representation - How Scheme types are represented in C++
// 3. (GC) Garbage collector
// 4. (RT) Runtime
// 5. (READ) Expression reader and source code tracking
// 6. (CC) Compiler
// 7. (VM) Virtual Machine
// 8. (UTIL) Utilities
// 9. (STD) Standard Scheme functions implemented in C++

// (PRELUDE)

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
#include <vector>

#if defined(_LP64)
# define ODD_64_BIT 1
#else
# define ODD_64_BIT 0
#endif

// Assertions. Since these can be quite expensive, they're only enabled when
// PIP_DEBUG is explicitly defined.
#ifdef PIP_DEBUG
# define PIP_ASSERT(x) (assert(x))
#else
# define PIP_ASSERT(x) ((void)0)
#endif

// Try to give a graceful error message when internal errors occur
#define PIP_FAIL0(desc) \
  std::cerr << desc << std::endl; \
  PIP_ASSERT(!"failure");

#define PIP_FAIL(desc, ret) \
  PIP_FAIL0(desc); \
  return (ret); 

// Checking for exceptions
#define PIP_CHECK(x) if((x)->active_exception()) return (x);

// Trace messages
#define PIP_GC_MSG(x) std::cout << "GC: " << x << std::endl
#define PIP_CC_MSG(x) \
    for(size_t _x = (depth); _x; _x--) \
      std::cout << '>'; std::cout << "CC: " << x << std::endl;
#define PIP_CC_VMSG(x) PIP_CC_MSG(x)
#define PIP_CC_EMIT(x) PIP_CC_MSG((last_insn) << ": " << x)

#define PIP_VM_MSG(x) \
  for(size_t _x = (vm_depth); _x != 1; _x--) \
    std::cout << '>'; \
  std::cout << "VM: " << x << std::endl
#define PIP_VM_VMSG(y) PIP_VM_MSG("(stack: " << f.si << ") " << y)

// Heap alignment is both the default size of the heap and the threshold for
// allocating large objects separate from the normal heap
#ifndef PIP_HEAP_ALIGNMENT
# define PIP_HEAP_ALIGNMENT 4096
#endif

// The "load factor" is a percentage used to determine when to grow the heap.
// When the live objects in memory after a collection exceed this percentage,
// the heap is grown.
#ifndef PIP_LOAD_FACTOR
# define PIP_LOAD_FACTOR 77
#endif

namespace pip {

struct State;
struct Value;
struct String;
struct Symbol;
struct VectorStorage;
struct Table;

std::ostream& operator<<(std::ostream& os, pip::Value* x);
std::ostream& print_table(std::ostream& os, Table* t);

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

struct SourceInfo {
  SourceInfo(): file(0), line(0) {}
  unsigned file, line;
};

///// (OBJ) Object representation 

// <--
// The first thing a language implementation needs to do is determine how to
// represent the language's values in memory. It's hard to interact with
// objects when you don't even know what they look like! 

// So how do we represent a Scheme object (dynamically typed) with a C++
// structure? The easiest way would be to take advantage of C++'s polymorphism.
// But I've decided to go in a different direction, for performance reasons.
// Pip values can be either immediate (they are contained directly within an
// integer) or heap-allocated (they are referenced by a pointer). In any case,
// these values are generally represented and passed around as "Value*".

// Why would we want to have "immediate" values? To save on allocating and
// dereferencing pointers when dealing with basic constants (such as boolean
// values, or most numbers). The downside of this approach is that pointers
// cannot be safely dereferenced without checking types first, but since we're
// dealing with dynamic objects, we generally have to check types before doing
// anything anyway.

// How do we determine an object's type? First, we have to check its bits. Each
// Value* has the following bit layout

// 0000 0000 - False, or #f
// .... ..10 - For constants
// .... ...1 - For fixed-point numbers
// .... ..00 - For real pointers

// In addition, heap-allocated objects have a type field. This is all handled
// by methods within the Value class. (Oddly enough, though we can't
// dereference a Value* safely, we can execute methods on it)

// Beyond that, objects are largely represented just like you'd think they
// would be. For instance, a Pair contains two pointers.
// -->

// Cast with assertions (will cause double evaluation; do not give an argument
// with side effects)
#define PIP_CAST(klass, x) \
  (PIP_ASSERT((x)->get_type() == pip::klass ::CLASS_TYPE), (klass *) (x))

// Constant values 
#define PIP_FALSE ((pip::Value*) 0)
#define PIP_TRUE ((pip::Value*) 2)
#define PIP_NULL ((pip::Value*) 6)
#define PIP_EOF ((pip::Value*) 10)
#define PIP_UNSPECIFIED ((pip::Value*) 18)

// C++ object types (which may not match up exactly with Scheme predicates)

// NOTE: Adding a type requires several modifications; search for "TYPENOTE"
enum Type {
  TRANSIENT,
  PAIR,
  VECTOR,
  VECTOR_STORAGE,
  BLOB,
  STRING,
  SYMBOL,
  EXCEPTION,
  BOX,
  // Functions
  PROTOTYPE,
  CLOSURE,
  UPVALUE,
  NATIVE_FUNCTION,
  TABLE,
  // Immediate values
  FIXNUM,
  CONSTANT,
};

std::ostream& operator<<(std::ostream& os, Type& t);

struct Value {
  // The space that's been allocated for this object, for the garbage
  // collector.
  size_t size;
  // The 'header' field contains an object's type, the garbage collector mark,
  // and other information that can be represented with a bit or two (such as
  // whether a Pair has source code information attached)
  unsigned header;

  // Headers have the following format
  // SSSS SSSM TTTT TTTT
  // Where S is 0 or an object-specific flag
  // M is the garbage collector mark
  // T is the object's type
  
  // The following methods deal with size and type and are mostly for internal
  // use

  // Get the bits of a Value*
  ptrdiff_t bits() const { return (ptrdiff_t) this; }
  
  // Type predicates
  
  // Check for ...1
  bool fixnump() const { return bits() & 1; }
  // Check for 0 or ..10 
  bool constantp() const { return !this || (bits() & 3) == 2; }
  // Check for ..00 (and not null)
  bool pointerp() const { return this && (bits() & 3) == 0; }
  // Anything that isn't a pointer is immediate
  bool immediatep() const { return !pointerp(); }
  bool applicablep() const {
    switch(get_type()) {
      case CLOSURE:
      case PROTOTYPE:
      case NATIVE_FUNCTION: return true;
      default: return false;
    }
  }

  // Methods for interacting with the header safely
  int get_header_bit(Type type, int bit) const {
    PIP_ASSERT(pointerp()); PIP_ASSERT(get_type() == type);
    return header & bit;
  }

  void set_header_bit(Type type, int bit) {
    PIP_ASSERT(get_type() == type);
    header += bit;
  }

  // Set the header's type and mark (for the garbage collector)
  void set_header_unsafe(unsigned char type, bool mark, size_t mark_bit) {
    header = (mark ? (mark_bit << 8) : (!mark_bit << 8)) + type;
  }

  // Interacting with marks and types
  int get_mark_unsafe() const { return (header & (1 << 8)) != 0; }
  size_t get_size_unsafe() const { return size; }
  void set_size_unsafe(size_t size_) { size = size_; }
  void flip_mark_unsafe() { header += get_mark_unsafe() ? -256 : 256; }
  void set_type_unsafe(int type) { header = (get_mark_unsafe() << 8) + type; }

  // Get the type of a heap-allocated object
  unsigned char get_type_unsafe() const { return header & 255; };

  // Safely determine the type of an object
  Type get_type() const {
    return fixnump() ? FIXNUM :
      (constantp() ? CONSTANT : static_cast<Type>(get_type_unsafe()));
  }

  // Type specific methods: simple getters and setters, object flags

  // Fixnums

  static Value* make_fixnum(ptrdiff_t n) { return (Value*) ((n << 1) + 1); }
  ptrdiff_t fixnum_value() const { return bits() >> 1; }

  // Pairs

  // True when the Pair has source code information attached
  static const int PAIR_HAS_SOURCE_BIT = 1 << 15;

  bool pair_has_source() const {
    return get_header_bit(PAIR, PAIR_HAS_SOURCE_BIT);
  }

  Value* car() const;
  Value* cdr() const;
  Value* caar() const;
  Value* cadr() const;
  Value* cddr() const;
  Value* cdar() const;

  void set_car(Value*);
  void set_cdr(Value*);

  // Vectors
  Value** vector_data();
  Value* vector_ref(size_t i);
  void vector_set(size_t, Value*);
  size_t vector_length() const;
  VectorStorage* vector_storage() const;

  // Vector storage
  Value** vector_storage_data() const;

  // Blobs
  const char* blob_data() const;
  unsigned blob_length() const;
  // Templated functions for dealing with structured data within blobs
  template <class T> unsigned blob_length() const;
  template <class T> T blob_ref(unsigned) const;
  template <class T> void blob_set(unsigned, T& value) const;

  // Strings
  const char* string_data() const;
  unsigned string_length() const;

  // Symbols
  Value* symbol_name() const;
  Value* symbol_value() const;

  // Exceptions

  // True when an exception is active and should be passed on
  static const int EXCEPTION_ACTIVE_BIT = 1 << 15;
  
  bool active_exception() const {
    return get_type() == EXCEPTION && (header & EXCEPTION_ACTIVE_BIT);
  }

  Symbol* exception_tag() const;
  String* exception_message() const;
  Value* exception_irritants() const;

  // Boxes

  // True when a box contains source code information
  static const int BOX_HAS_SOURCE_BIT = 1 << 15;

  bool box_has_source() const {
    return get_header_bit(BOX, BOX_HAS_SOURCE_BIT);
  }

  Value* box_value() const;

  // Prototypes
  static const int PROTOTYPE_VARIABLE_ARITY_BIT = 1 << 15;

  bool prototype_variable_arity() const {
    return get_header_bit(PROTOTYPE, PROTOTYPE_VARIABLE_ARITY_BIT);
  }

  // Upvalues
  static const int UPVALUE_CLOSED_BIT = 1 << 15;

  bool upvalue_closed() const {
    return get_header_bit(UPVALUE, UPVALUE_CLOSED_BIT);
  }

  Value* upvalue() const;
  void upvalue_set(Value*);
  void upvalue_close();

  // Native functions
  static const int NATIVE_FUNCTION_VARIABLE_ARITY_BIT = 1 << 15;

  bool native_function_variable_arity() const { 
    return get_header_bit(NATIVE_FUNCTION, NATIVE_FUNCTION_VARIABLE_ARITY_BIT);
  }

  // Tables
  static const int TABLE_ACT_AS_SET_BIT = 1 << 15;
  static const int TABLE_WEAK_BIT = 1 << 14;

  bool table_act_as_set() const {
    return get_header_bit(TABLE, TABLE_ACT_AS_SET_BIT);
  }

  bool table_weak() const { return get_header_bit(TABLE, TABLE_WEAK_BIT); }
};

struct Pair : Value {
  Value *car, *cdr;
  // Source code information; only available when pair_has_source() is true
  SourceInfo src;
  
  static const Type CLASS_TYPE = PAIR;
};

Value* Value::car() const {
  PIP_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->car;
}

Value* Value::cdr() const {
  PIP_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr;
}

Value* Value::caar() const {
  PIP_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->car->car();
}

Value* Value::cadr() const {
  PIP_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr->car();
}

Value* Value::cddr() const {
  PIP_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr->cdr();
}

Value* Value::cdar() const {
  PIP_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->car->cdr();
}

void Value::set_car(Value* car_) { 
  PIP_ASSERT(get_type() == PAIR);
  static_cast<Pair*>(this)->car = car_;
}

void Value::set_cdr(Value* cdr_) {
  PIP_ASSERT(get_type() == PAIR);
  static_cast<Pair*>(this)->cdr = cdr_;
}

struct VectorStorage : Value {
  unsigned length;
  Value* data[1];

  static const Type CLASS_TYPE = VECTOR_STORAGE;
};

Value** Value::vector_storage_data() const {
  PIP_ASSERT(get_type() == VECTOR_STORAGE);
  return (Value**)static_cast<const VectorStorage*>(this)->data;
}

struct Vector : Value {
  VectorStorage* storage;
  unsigned capacity;

  static const Type CLASS_TYPE = VECTOR;
};

VectorStorage* Value::vector_storage() const {
  PIP_ASSERT(get_type() == VECTOR);
  return static_cast<const Vector*>(this)->storage;
}
Value** Value::vector_data() { return vector_storage()->data; }
Value* Value::vector_ref(size_t i) { return vector_storage()->data[i]; }
void Value::vector_set(size_t i, Value* v) { vector_storage()->data[i] = v; }
size_t Value::vector_length() const { return vector_storage()->length; }

struct Blob : Value {
  unsigned length;
  char data[1];

  static const Type CLASS_TYPE = BLOB;
};

const char* Value::blob_data() const {
  PIP_ASSERT(get_type() == BLOB); return static_cast<const Blob*>(this)->data;
}

unsigned Value::blob_length() const {
  PIP_ASSERT(get_type() == BLOB);
  return static_cast<const Blob*>(this)->length;
}

template <class T>
unsigned Value::blob_length() const {
  PIP_ASSERT(get_type() == BLOB); 
  return static_cast<const Blob*>(this)->length / sizeof(T); 
}

template <class T> 
T Value::blob_ref(unsigned i) const { 
  PIP_ASSERT(get_type() == BLOB); return ((T*) 
      static_cast<const Blob*>(this)->data)[i]; 
}

template <class T> 
void Value::blob_set(unsigned i, T& val) const {
  PIP_ASSERT(get_type() == BLOB);
  ((T*) static_cast<const Blob*>(this)->data)[i] = val;
}

struct String : Blob {
  static const Type CLASS_TYPE = STRING;
};

const char* Value::string_data() const {
  PIP_ASSERT(get_type() == STRING); return static_cast<const String*>(this)->data;
}
unsigned Value::string_length() const {
  PIP_ASSERT(get_type() == STRING);
  return static_cast<const String*>(this)->length;
}

struct Symbol : Value {
  Value *name, *value;

  static const Type CLASS_TYPE = SYMBOL;
};

Value* Value::symbol_name() const {
  PIP_ASSERT(get_type() == SYMBOL);
  return static_cast<const Symbol*>(this)->name;
}
Value* Value::symbol_value() const {
  PIP_ASSERT(get_type() == SYMBOL);
  return static_cast<const Symbol*>(this)->value;
}

struct Exception : Value {
  Symbol* tag;
  String* message;
  Value* irritants;
  
  static const Type CLASS_TYPE = EXCEPTION;
};

Symbol* Value::exception_tag() const {
  PIP_ASSERT(get_type() == EXCEPTION);
  return static_cast<const Exception*>(this)->tag; 
};

String* Value::exception_message() const {
  PIP_ASSERT(get_type() == EXCEPTION);
  return static_cast<const Exception*>(this)->message;
};

Value* Value::exception_irritants() const {
  PIP_ASSERT(get_type() == EXCEPTION);
  return static_cast<const Exception*>(this)->irritants; 
};

struct Box : Value {
  Value* value;
  SourceInfo src;

  static const Type CLASS_TYPE = BOX;
};

Value* Value::box_value() const {
  PIP_ASSERT(get_type() == BOX);
  return static_cast<const Box*>(this)->value;
};

// Prototype for a function containing constants, code, and debugging
// information. May be executed directly if the function does not reference
// free variables.
struct Prototype : Value {
  Vector* constants;
  Blob* code;
  Blob* debuginfo;
  // A list of local free variables referenced by other functions, which will
  // be converted into upvalues when the function ends
  Blob* local_free_variables;
  // A list of upvalues, which will be used to turn this into a closure if
  // necessary
  Blob* upvalues;
  // The function's name, if available, or #f if not
  String* name;
  unsigned stack_max;
  unsigned locals;
  unsigned local_free_variable_count;
  unsigned arguments;

  static const Type CLASS_TYPE = PROTOTYPE;
};

// A closure, aka a function which references free variables
struct Closure : Value {
  Prototype* prototype;
  // Vector containing Upvalues
  VectorStorage* upvalues;

  static const Type CLASS_TYPE = CLOSURE;
};

// A reference to a free variable; used to support closures
struct Upvalue : Value {
  union { 
    Value** local;
    Value* converted;
  };

  static const Type CLASS_TYPE = UPVALUE;
};

Value* Value::upvalue() const {
  Upvalue* u = PIP_CAST(Upvalue, this);
  return upvalue_closed() ? u->converted : *(u->local);
}

void Value::upvalue_set(Value* v) {
  Upvalue* u = PIP_CAST(Upvalue, this);
  if(upvalue_closed())
    u->converted = v;
  else
    (*u->local) = v;
}

void Value::upvalue_close() {
  Upvalue* u = PIP_CAST(Upvalue, this);
  set_header_bit(UPVALUE, UPVALUE_CLOSED_BIT);
  u->converted = *u->local;
}

struct NativeFunction : Value {
  typedef Value* (*ptr_t)(State&, unsigned, Value* args[]);

  unsigned arguments;
  ptr_t pointer;

  static const Type CLASS_TYPE = NATIVE_FUNCTION;
};

struct Table : Value {
  static const int LOAD_FACTOR = 70;

  // Chain format: vector of lists, where lists are ((key . value) ...)
  // With as many key/value pairs as necessary for collisions
  VectorStorage* chains;
  unsigned char size_log2;
  unsigned entries, max_entries;
  
  static const Type CLASS_TYPE = TABLE;
};

///// GARBAGE COLLECTOR TRACKING

// Handles are auto-tracked heap-allocated pointers
template <class T>
struct Handle {
  Handle(State& state_): state(state_), ref(0) {
    initialize();
  }

  Handle(State& state_, T* ref_): state(state_), ref(ref_) { 
    initialize();
  }

  void initialize();
  ~Handle();
 
   State& state;
  T* ref;
  Handle<Value> *previous, *next;

  T* operator*() const { return ref; }
  T* operator->() const { return ref; }
  void operator=(T* ref_) { ref = ref_; }  
};

// This structure is used in the GC tracking macros to take a normal
// pointer to a Pip value of any type and obtain a Value** pointer to it
struct FrameHack {
  // TYPENOTE
#define PIP_(t) FrameHack(t *& ref): ref((Value**) &ref) {}
  PIP_(Value)
  PIP_(Pair)
  PIP_(Blob)
  PIP_(Symbol)
  PIP_(Vector)
  PIP_(VectorStorage)
  PIP_(String)
  PIP_(Exception)
  PIP_(Box)
  PIP_(Prototype)
  PIP_(Closure)
  PIP_(Upvalue)
  PIP_(NativeFunction)
  PIP_(Table)
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

// Frame with explicit state argument
#define PIP_E_FRAME(state, ...) \
  pip::FrameHack __pip_frame_hacks[] = { __VA_ARGS__ }; \
  pip::Frame __pip_frame((state), (FrameHack*) __pip_frame_hacks, \
      sizeof(__pip_frame_hacks) / sizeof(pip::FrameHack))

// For general use

// Assumes there is a variable State& state
#define PIP_FRAME(...) PIP_E_FRAME((state), __VA_ARGS__)
#define PIP_S_FRAME(...) PIP_E_FRAME((*this), __VA_ARGS__)

struct State {
  struct VMFrame;

  ///// (GC) Garbage collector

  // NOTE: Garbage collector tracking macros are declared above the State
  // structure

  // <--
  // Pip's garbage collector is a mark-and-don't-sweep collector. The allocator
  // is a first-fit allocator: it allocates memory from the first appropriately
  // sized free chunk of memory it can find. It marks every chunk of memory it
  // sweeps over (or, if the chunk is already marked, it skips it). When the
  // allocator reaches the end of the heap, the meaning of the mark bit is
  // flipped, and the entire heap is considered garbage. Then the collector
  // immediately marks all live memory, and we know which objects are marked.
  // The benefit of a mark-and-don't-sweep collector is that the work is spread
  // out amongst allocations and should (in most cases) result in no large
  // pauses to the program. It requires no external data structures; the only
  // overhead being that each object needs a "size" and "mark" field (and in
  // Pip's case, the mark field is stored alongside type and other flags in a
  // space efficient manner). The main downside is fragmentation.

  // How does Pip mark live memory? It starts with the program's "roots" and
  // marks all their children and so on. But how do we find the roots? They are
  // explicitly registered with the garbage collector using two methods: Frames
  // and Handles. A Frame is a stack-allocated structure that keeps track of as
  // many variables as needed. You create a frame in each function you want to
  // track variables in like so: PIP_FRAME(var1, var2). A Handle is a
  // heap-allocated structure that keeps track of one variable, and is useful
  // when your variables have lifetimes beyond the execution of a single
  // function.

  // How does Pip get memory from the operating system? It starts by using a
  // small amount of memory (4 kilobytes). If, after a collection, more than a
  // certain amount of memory is in use (the "load factor", by default 77%), it
  // allocates twice as much memory to avoid collecting too often. It can also
  // allocate more memory if a very large object (over 4kb) is allocated.

  // In addition to normal collection, Pip has support for doing a full
  // compaction on the heap, reducing fragmentation to 0. Currently, this can
  // only be used manually and only at the top-level of program execution. So
  // although it might not benefit a normal program's execution, it could be
  // used eg while a game is loading levels to prevent fragmentation over time
  // --> 

  static const size_t POINTER_ALIGNMENT = sizeof(void*);

  struct Block {
    size_t size;
    char *begin, *end;
  
    Block(size_t size, bool mark, int mark_bit) {
      begin = (char*) malloc(size);
      end = begin + size;
      ((Value*) begin)->set_header_unsafe(TRANSIENT, mark, mark_bit);
      ((Value*) begin)->set_size_unsafe(size);
    }

    ~Block() {
      free(begin);
    }
  };

  std::vector<Block*> blocks;
  Handle<Value>* handle_list;
  std::vector<VMFrame*> vm_frames;
  Frame* frame_list;
  bool collect_before_every_allocation;
  int mark_bit;
  size_t heap_size, block_cursor, live_at_last_collection, collections;
  char* sweep_cursor;

  bool markedp(Value* x) {
    return x->get_mark_unsafe() == mark_bit;
  }

  void recursive_mark(Value* x) {
    // TYPENOTE
    // Done in a while loop to save stack space
    while(x->pointerp() && !markedp(x)) {
      live_at_last_collection += x->get_size_unsafe();
      x->flip_mark_unsafe();

      switch(x->get_type_unsafe()) {
        // Note that in order to save a little redunancy, objects are marked
        // based on their structure (eg all objects with two pointers are
        // marked as though they were pairs)

        // Atomic
        case NATIVE_FUNCTION: case STRING: case BLOB:
          return;
        // One pointer
        case TABLE:
        case VECTOR:
        case BOX:
          x = static_cast<Box*>(x)->value;
          continue;
        // Two pointers 
        case CLOSURE:
        case PAIR:
        case SYMBOL:
          recursive_mark(static_cast<Pair*>(x)->cdr);
          x = static_cast<Pair*>(x)->car;
          continue;
        // Three pointers
        case EXCEPTION:
          recursive_mark(static_cast<Exception*>(x)->tag);
          recursive_mark(static_cast<Exception*>(x)->message);
          x = static_cast<Exception*>(x)->irritants;
          continue;
        // Four pointers
        case PROTOTYPE:
          recursive_mark(static_cast<Prototype*>(x)->code);
          recursive_mark(static_cast<Prototype*>(x)->debuginfo);
          recursive_mark(static_cast<Prototype*>(x)->local_free_variables);
          recursive_mark(static_cast<Prototype*>(x)->upvalues);
          recursive_mark(static_cast<Prototype*>(x)->name);
          x = static_cast<Prototype*>(x)->constants;
          continue;
        // Special types
        case VECTOR_STORAGE: {
          VectorStorage* s = static_cast<VectorStorage*>(x);
          if(s->length) {
            for(size_t i = 0; i != s->length - 1; i++) {
              recursive_mark(s->data[i]);
            }
            x = s->data[s->length - 1];
          }
          continue;
        }
        case UPVALUE: { 
          x = x->upvalue();
          continue;
        }
        default:
          PIP_FAIL0("recursive_mark got bad type " << x->get_type());
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

  // If collection is called early, finish iterating over and marking the heap
  void finish_mark() {
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
  }

  // Mark all live memory
  void mark_heap() {
    // Symbols are roots (for now)
    for(Frame* f = frame_list; f != NULL; f = f->previous)
      for(size_t i = 0; i != f->root_count; i++)
        recursive_mark(*f->roots[i]);

    for(Handle<Value>* i = handle_list; i != NULL; i = i->previous)
      recursive_mark(i->ref);
    
    for(size_t i = 0; i != vm_frames.size(); i++) {
      VMFrame* f = vm_frames[i];
      // Mark prototype
      recursive_mark(f->p);
      // Mark closure
      recursive_mark(f->c);
      // Mark function stack
      for(size_t j = 0; j != f->si; j++) {
        recursive_mark(f->stack[j]);
      }
      // Mark local variables
      for(size_t j = 0; j != f->p->locals; j++) {
        recursive_mark(f->locals[j]);
      }
      // Mark upvalues (if necessary)
      for(size_t j = 0; j != f->p->local_free_variable_count; j++) {
        recursive_mark(f->upvalues[j]);
      }
    }

    // Handle the symbol table, which is sort of an ad hoc weak table
    for(size_t i = 0; i != symbol_table->chains->length; i++) {
      Value* cell = symbol_table->chains->data[i];
      Value* previous = 0;
      while(cell->get_type() == PAIR) {
        // If the value has been collected
        if(!markedp(cell->cdar())) {
          // If this is the head of the linked list
          if(cell == symbol_table->chains->data[i]) {
            // remove from front
            symbol_table->chains->data[i] = cell->cdr();
            previous = 0;
            cell = cell->cdr();
            continue;
          }
          // Remove cell from previous list
          else if(previous) {
            previous->set_cdr(cell->cdr());
            cell = cell->cdr();
            continue;
          }
        }
        // Cell is alive, keep going
        previous = cell;
        cell = cell->cdr();
      } 
      if(symbol_table->chains->data[i] == PIP_NULL)
        symbol_table->chains->data[i] = PIP_FALSE;
    }

    // Now that all the collected entries have been removed, we can mark the
    // table
    recursive_mark(symbol_table);
  }
  
  void reset_sweep_cursor() {
    sweep_cursor = blocks[0]->begin;
    block_cursor = 0;
  }

  void collect(size_t request = 0, bool force_request = false) {
    collections++;

    // If collection is called early, we have to sweep over everything
    // and make sure it's marked as used
    finish_mark();

    // Reset sweep cursor
    reset_sweep_cursor();

    // If growth is necessary

    // Determine whether we should grow
    // This will happen if
    // a) Most of the heap is in use
    // b) A collection has failed to free up enough space for an allocation

    size_t pressure = (live_at_last_collection * 100) / heap_size;
    if(pressure >= PIP_LOAD_FACTOR || force_request) {
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

      PIP_GC_MSG("growing heap from " <<
          FriendlySize(heap_size - new_block_size)
          << " to " << FriendlySize(heap_size)
          << " because of " <<
          (pressure >= PIP_LOAD_FACTOR ? "pressure" : "allocation failure"));
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
    mark_heap();
  }

  // Allocation
  Value* allocate(Type type, size_t size) {
    if(collect_before_every_allocation) {
      collect();
    }
  
    // This function calls findfree and does some minor tweaks before
    // and after

    // Align along pointer-sized boundary to ensure that our immediate
    // value scheme will work.
    size = align(POINTER_ALIGNMENT, size);

    if(size >= PIP_HEAP_ALIGNMENT) {
      size = align(PIP_HEAP_ALIGNMENT, size);
      Block* block = new Block(size, false, mark_bit);
      blocks.push_back(block);
      Value* x = (Value*) block->begin;
      x->set_type_unsafe(type);
      x->flip_mark_unsafe();
      PIP_ASSERT(markedp(x));

      heap_size += size;

      PIP_GC_MSG("growing heap from " << FriendlySize(heap_size - size)
        << " to " << FriendlySize(heap_size)
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
          // TODO: A graceful out
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
  
  // The actual entry point of the garbage collector
  template <class T> T* allocate(ptrdiff_t additional = 0) {
    return static_cast<T*>(allocate(static_cast<Type>(T::CLASS_TYPE),
                           sizeof(T) + additional));
  }

  ///// COMPACTION

  // Determine the minimum size of an object; used to remove fragmentation
  // while compacting
  static size_t minimum_size(Value* x) {
    // TYPENOTE
    size_t size = 0;
    switch(x->get_type()) {
      // Simple cases
      case SYMBOL: size = sizeof(Symbol); break;
      case VECTOR: size = sizeof(Vector); break;
      case EXCEPTION: size = sizeof(Exception); break;
      case CLOSURE: size = sizeof(Closure); break;
      case PROTOTYPE: size = sizeof(Prototype); break; 
      case UPVALUE: size = sizeof(Upvalue); break;
      case NATIVE_FUNCTION: size =  sizeof(NativeFunction); break;
      case TABLE: size = sizeof(Table); break;
      case PAIR:
        size = sizeof(Pair) + src_size<Pair>(x->pair_has_source());
        break;
      case BOX:
        size = sizeof(Box) + src_size<Box>(x->box_has_source());
        break;
      case VECTOR_STORAGE:
        size = sizeof(VectorStorage) +
               array_size<Value*>(static_cast<VectorStorage*>(x)->length);
        break;
      case STRING: {
        size = sizeof(String) + 
               array_size<unsigned char>(static_cast<String*>(x)->length);
        break;
      }
      case BLOB:
        size = sizeof(Blob) + 
               array_size<unsigned char>(static_cast<Blob*>(x)->length);
        break;
        // Should not occur
      case TRANSIENT: case FIXNUM: case CONSTANT:
        PIP_FAIL0("minimum_size type should not occur: " << x->get_type());
     }
     return align(POINTER_ALIGNMENT, size);
   }

  void update_forward(Value** ref) {
    Value* obj = *ref;
    // If object is immedate or pointer has already been updated, no need to
    // deal with it
    if(obj->immediatep() || obj->header != TRANSIENT) return;
    // Update pointer
    (*ref) = ((Value*) obj->size);
  }

  void compact() {
    // Quick collection
    finish_mark();
    reset_sweep_cursor();
    mark_bit = !mark_bit;
    mark_heap();

    // Create a new block to copy the heap to
    Block* heap = new Block(align(PIP_HEAP_ALIGNMENT, heap_size),
                            false, mark_bit);

    // Loop over live memory, allocating from the new block. The new allocation
    // is called the "forwarded object" and the object's 'size" field is
    // re-used to store a pointer to this forwarded object. (The newly
    // allocated object contains the size information, for when it's needed)

    char* bump = heap->begin;

    PIP_GC_MSG("copying heap");
    // TODO: Sweep code is duplicated 3-4 times. 
    while(block_cursor != blocks.size()) {
      Block* b = blocks[block_cursor];
      while(sweep_cursor != b->end) {
        Value* sweep_cursor_v = (Value*) sweep_cursor;

        if(markedp(sweep_cursor_v)) {
          // Bump allocate from the new heap
          Value* copy = (Value*) bump;
          size_t min_size = minimum_size(sweep_cursor_v);
          bump += min_size;

          memcpy(copy, sweep_cursor_v, min_size);
          copy->size = min_size;
          // Increment before we delete the object's size
          sweep_cursor += sweep_cursor_v->size;
          // Install forwarding pointer
          sweep_cursor_v->size = (size_t) copy;

          sweep_cursor_v->header = TRANSIENT;
        } else {
          sweep_cursor += sweep_cursor_v->get_size_unsafe();
        }
      }
      block_cursor++;
      if(block_cursor != blocks.size())
        sweep_cursor = blocks[block_cursor]->begin;
    }

    // Now that every object has been copied, we just need to update pointers.

    // First we'll sweep through the program's roots, then we'll sweep over the
    // heap (which can be easily iterated over at this point),  checking every
    // object's field for a forwarded pointer.

    PIP_GC_MSG("updating roots with new pointers");

    for(Frame* f = frame_list; f != NULL; f = f->previous)
      for(size_t i = 0; i != f->root_count; i++)
        update_forward(f->roots[i]);

    for(Handle<Value>* i = handle_list; i != NULL; i = i->previous)
      update_forward(&i->ref);
    
    for(size_t i = 0; i != vm_frames.size(); i++) {
      VMFrame* f = vm_frames[i];
      // Mark prototype
      update_forward((Value**) &f->p);
      // Mark closure
      update_forward((Value**) &f->c);
      // Mark function stack
      for(size_t j = 0; j != f->si; j++) {
        update_forward(&f->stack[j]);
      }
      // Mark local variables
      for(size_t j = 0; j != f->p->locals; j++) {
        update_forward(&f->locals[j]);
      }
      // Mark upvalues (if necessary)
      for(size_t j = 0; j != f->p->local_free_variable_count; j++) {
        update_forward((Value**)&f->upvalues[j]);
      }
    }

    update_forward((Value**) &symbol_table);

    PIP_GC_MSG("updating heap with new pointers");
    char* sweep = heap->begin;
    while(sweep != bump) {
      Value* x = (Value*) sweep;
      sweep += x->size;
#define PIP_FWD(type, field) update_forward((Value**) &(((type*) x)->field))
      switch(x->get_type_unsafe()) {
        // TYPENOTE
        // Atomic
        case NATIVE_FUNCTION: case STRING: case BLOB:
          continue;
        // One pointer
        case VECTOR: case BOX: case TABLE:
          PIP_FWD(Box, value);
          continue;
        // Two pointers
        case CLOSURE: case PAIR: case SYMBOL:
          update_forward(&((Pair*) x)->car);
          update_forward(&((Pair*) x)->cdr);
          continue;
        // Three pointers
        case EXCEPTION:
          PIP_FWD(Exception, tag);
          PIP_FWD(Exception, message);
          PIP_FWD(Exception, irritants);
          continue;
        // Five pointers
        case PROTOTYPE:
          PIP_FWD(Prototype, code);
          PIP_FWD(Prototype, debuginfo);
          PIP_FWD(Prototype, local_free_variables);
          PIP_FWD(Prototype, upvalues);
          PIP_FWD(Prototype, name);
          continue;
        case VECTOR_STORAGE: {
          VectorStorage* s = static_cast<VectorStorage*>(x);
          if(s->length) {
            for(size_t i = 0; i != s->length; i++) {
              update_forward(&s->data[i]);
            }
          }
          continue;
        }
        case UPVALUE:
          if(x->upvalue_closed()) {
            update_forward(((Upvalue*) x)->local);
          } else {
            update_forward(&((Upvalue*) x)->converted);
          }
          continue;
        default: case TRANSIENT: case FIXNUM: case CONSTANT:
          PIP_FAIL0("compact encountered bad type: " << x->get_type()); 
#undef PIP_FWD
      }
    }

    PIP_GC_MSG("forwarded all program roots");
    
    // Delete heap and replace with new heap
    delete_blocks();
    blocks.push_back(heap);
    reset_sweep_cursor();

    // Note the size of the hole at the end of the heap
    // Ensure we have enough room at the end for a Value
    PIP_ASSERT((size_t)(heap->end - bump) >= align(POINTER_ALIGNMENT,
          sizeof(Value*)));
    // Determine the size
    Value* hole = (Value*) bump;
    hole->header = TRANSIENT;
    hole->size = (heap->end - bump);
  }

  ///// (RT) Runtime

  // <--
  // Each instance of the Pip runtime is contained within the rather monolithic
  // State structure.  Pip is completely re-entrant. 
  // -->

  State(): 
    // Garbage collector
    handle_list(0),
    frame_list(0), 
    collect_before_every_allocation(false), mark_bit(1),
    heap_size(PIP_HEAP_ALIGNMENT),
    block_cursor(0),  live_at_last_collection(0), collections(0),
    sweep_cursor(0),

    // Reader
    source_counter(1),
   
    // Compiler

    // An environment containing special forms, core functions, and all the
    // other building blocks necessary for Pip
    core_env(*this),
    
    // Virtual machine
    vm_depth(0),
    vm_trampoline_function(0) {

    // Initialize garbage collector
    Block* first = new Block(PIP_HEAP_ALIGNMENT * 2, false, mark_bit);
    blocks.push_back(first);
    sweep_cursor = first->begin;

    // Initialize globals
    static const char* global_symbols[] = {
      // Compiler symbols
      "special",
      "variable",
      "upvalue",
      // Exception tags
      "#pip#read",
      "#pip#compile",
      "#pip#eval",
      "#pip#table",
      // Special forms
      "define",
      "set!",
      "begin",
      "lambda",
      "#named-lambda",
      "if",
      "define-syntax",
      "let-syntax",
      "letrec-syntax",
      "define-library",
      "quote",
      // Other
      "quasiquote",
      "unquote",
      "unquote-splicing",
      "import",
      "export",
      "rename",
      "include",
      "#delegates",

    };

    // Allocate the symbol table, with a fair bit of space because we're going
    // to stuff it full of those symbols up there
    symbol_table = make_table(8);

    for(size_t i = 0; i != GLOBAL_SYMBOL_COUNT; i++) {
      Value* s = make_symbol(global_symbols[i]);
      Handle<Value>* v = new Handle<Value>(*this, s);
      globals.push_back(v);
    }

    // Initialize reader
    source_names.push_back("unknown");
    source_contents.push_back(NULL);

    // Initialize core environment for compiler
    core_env = make_env();
    for(size_t i = S_DEFINE; i != S_QUOTE+1; i++) {
      env_define(*core_env, global_symbol(static_cast<Global>(i)),
                 global_symbol(S_SPECIAL));
    }

    initialize_builtin_functions();
  }

  ~State() {
    for(size_t i = 0; i != globals.size(); i++)
      delete globals[i]; 

    delete_blocks();

    for(size_t i = 0; i != source_contents.size(); i++)
      delete source_contents[i];
  }

  void delete_blocks() {
    for(size_t i = 0; i != blocks.size(); i++)
      delete blocks[i];
    blocks.clear();
  }

  Table* symbol_table;
  std::vector<Handle<Value> *> globals;

  enum Global {
    S_SPECIAL,
    S_VARIABLE,
    S_UPVALUE,
    S_PIP_READ,
    S_PIP_COMPILE,
    S_PIP_EVAL,
    S_PIP_TABLE,
    S_DEFINE,
    S_SET,
    S_BEGIN,
    S_LAMBDA,
    S_NAMED_LAMBDA,
    S_IF,
    S_DEFINE_SYNTAX,
    S_LET_SYNTAX,
    S_LETREC_SYNTAX,
    S_DEFINE_LIBRARY,
    S_QUOTE,
    S_QUASIQUOTE,
    S_UNQUOTE,
    S_UNQUOTE_SPLICING,
    S_IMPORT,
    S_EXPORT,
    S_RENAME,
    S_INCLUDE,
    S_DELEGATES,
    GLOBAL_SYMBOL_COUNT
  };

  Symbol* global_symbol(Global g) {
    return static_cast<Symbol*>(**(globals[(size_t) g]));
  }

  void initialize_builtin_functions();

  ///// CONSTRUCTORS

  // Returns how much memory to subtract if an object will not contain source
  // code information
  template <class T> 
  static ptrdiff_t src_size(bool yes) {
    return yes ? 0 : -(sizeof(((T*)0)->src));
  }

  // Returns how much memory to use when an object contains an array at the end
  template <class T>
  static ptrdiff_t array_size(size_t elts) {
    return (sizeof(T) * elts) - sizeof(T);
  }

  Box* make_box(Value* value, bool has_source_info, unsigned file = 0,
                unsigned line = 0) {
    Box* box = 0;
    PIP_S_FRAME(value, box);
    box = allocate<Box>(src_size<Box>(has_source_info));
    box->value = value;
    if(has_source_info) {
      box->set_header_bit(BOX, Value::BOX_HAS_SOURCE_BIT);
      box->src.file = file;
      box->src.line = line;
    }
    return box;
  }

  Pair* cons(Value* car, Value* cdr, bool has_source_info = false,
            unsigned file = 0, unsigned line = 0) {
    Pair* p = NULL;
    PIP_S_FRAME(p, car, cdr);
    p = allocate<Pair>(src_size<Pair>(has_source_info));
    p->car = car;
    p->cdr = cdr;
    if(has_source_info) {
      p->set_header_bit(PAIR, Value::PAIR_HAS_SOURCE_BIT);
      p->src.file = file;
      p->src.line = line;
    }
    return p;
  }

  // Cons and copy source information from a third expression
  Pair* cons_source(Value* car, Value* cdr, Value* src_exp) {
    SourceInfo src;
    unwrap_source_info(src_exp, src);
    if(src.file) {
      return cons(car, cdr, true, src.file, src.line);
    } else {
      return cons(car, cdr);
    }
  }

  Blob* make_blob(ptrdiff_t length) {
    Blob* b = allocate<Blob>(array_size<unsigned char>(length));
    b->length = length;
    return b;
  }

  template <class T>
  Blob* make_blob(ptrdiff_t length) {
    return make_blob(sizeof(T) * length);
  }

  String* make_string(const std::string& cstr) {
    String* str = allocate<String>(cstr.length());
    cstr.copy(str->data, cstr.length());
    str->data[cstr.length()] = 0;
    str->length = cstr.length();
    return str;
  }

  String* make_string(String* copy) {
    String* str = allocate<String>(copy->string_length());
    memcpy(str->data, copy->data, copy->string_length());
    str->length = copy->length;
    return str;
  }

  Symbol* make_symbol(String* string) {
    Symbol* sym = 0;
    bool found;
    Value* search = table_get(symbol_table, string, found);
    if(found) {
      sym = PIP_CAST(Symbol, search);
    } else {
      // Symbol doesn't exist yet, intern it
      String* cpy = 0;
      PIP_S_FRAME(sym, string, cpy);
      sym = allocate<Symbol>();
      cpy = make_string(string);
      sym->name = cpy;
      table_insert(symbol_table, cpy, sym);
    }
    return sym;
  }

  Symbol* make_symbol(const std::string& str) {
    return make_symbol(make_string(str));
  }

  Vector* make_vector(unsigned capacity = 2) {
    capacity = capacity > 2 ? capacity : 2;
    Vector* v = 0;
    VectorStorage* s = 0;
    PIP_S_FRAME(v, s);
    v = allocate<Vector>();
    v->capacity = capacity;
    s = allocate<VectorStorage>(array_size<Value*>(capacity));
    v->storage = s;
    return v;
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

  void table_setup(Table* table, unsigned size_log2) {
    VectorStorage* chains = 0;
    PIP_S_FRAME(table, chains);
    // Clear entries 
    table->entries = 0;
    // Calculate size
    table->size_log2 = size_log2;
    unsigned size = 1 << size_log2;
    chains = vector_storage_realloc(size, 0);
    // Fill entries with #f
    chains->length = size;
    memset(chains->data, 0, array_size<Value*>(chains->length));
    // Note chains
    table->chains = chains;
    // Pre-calculate max entries
    table->max_entries = (Table::LOAD_FACTOR * size) / 100;
  }

  Table* make_table(unsigned size_log2 = 4) {
    VectorStorage* chains = 0;
    Table* table = 0;
    PIP_S_FRAME(table, chains);

    table = allocate<Table>();
    table_setup(table, size_log2);
    
    return table;
  }

  ///// BASIC FUNCTIONS

  static bool equals(Value* a, Value* b) {
    // TYPENOTE
    if(a->get_type() != b->get_type()) return false;
    switch(a->get_type()) {
      // Identity
      case FIXNUM:
      case CONSTANT:
      case SYMBOL:
        return a == b;
      case STRING:
        return strcmp(PIP_CAST(String, a)->data, PIP_CAST(String, b)->data)==0;
      default: break;
    }
    std::cerr << "equals not implemented for type " << a->get_type();
    PIP_ASSERT(!"equals not implemented for type");
    return false;
  }

  void defun(const std::string& cname, NativeFunction::ptr_t ptr,
             unsigned arguments, bool variable_arity) {
    NativeFunction* fn = 0;
    Symbol* name = 0;
    PIP_S_FRAME(fn, name);
    fn = allocate<NativeFunction>();
    fn->arguments = arguments;
    fn->pointer = ptr;
    if(variable_arity) 
      fn->set_header_bit(NATIVE_FUNCTION,
                         Value::NATIVE_FUNCTION_VARIABLE_ARITY_BIT);
    name = make_symbol(cname);
    env_define(*core_env, name, name);
    name->value = fn;
  }

  VectorStorage* vector_storage_realloc(size_t capacity, VectorStorage* old) {
    VectorStorage* data = 0;
    PIP_S_FRAME(old, data);

    data = allocate<VectorStorage>(array_size<Value*>(capacity));
    if(old) {
      data->length = old->length;
      memcpy(data->data, old->data, old->length * sizeof(Value*));
    }
    return data;
  }

  unsigned vector_append(Vector* vec, Value* value) {
    vec->storage->data[vec->storage->length++] = value;
    if(vec->storage->length == vec->capacity) {
      VectorStorage* s = 0;
      PIP_S_FRAME(vec, s);
      s = vector_storage_realloc(vec->capacity * 2, vec->storage);
      vec->capacity *= 2;
      vec->storage = s;
    }
    return vec->storage->length - 1;
  }

  static void unwrap_source_info(Value* exp, SourceInfo& info) {
    if(exp->get_type() == PAIR && exp->pair_has_source()) {
      info = PIP_CAST(Pair, exp)->src;
    } else if(exp->get_type() == BOX && exp->box_has_source()) {
      info = PIP_CAST(Box, exp)->src;
    }
  }

  void format_source_error(Value* exp, const std::string& msg,
                          std::string& out) {
    SourceInfo info;
    unwrap_source_info(exp, info);
    if(info.file) {
      std::ostringstream ss;
      ss << source_names[info.file] << ':' << info.line << ": " << msg << 
        std::endl << "  " << source_contents[info.file]->at(info.line-1);
      out = ss.str();
    } else {
      out = msg;
    }
  }

  // Lists

  // Returns true if something is a true list (either () or a list with no dots in it)
  static bool listp(Value* lst) {
    if(lst == PIP_NULL) return true;
    if(lst->get_type() != PAIR) return false;
    while(lst->get_type() == PAIR) {
      lst = lst->cdr();
      if(lst == PIP_NULL) return true;
    }
    return false;
  }

  // Returns length of list
  static size_t length(Value* lst) {
    size_t i = 0;
    while(lst->get_type() == PAIR) {
      i++;
      lst = lst->cdr();
    }
    return i;
  }

  // A helpful function for building lists
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

  ///// HASH TABLES

  static ptrdiff_t wang_integer_hash(ptrdiff_t key) {
#if ODD_64_BIT
    key = (~key) + (key << 21);
    key = key ^ (key >> 24);
    key = (key + (key << 3)) + (key << 8); 
    key = key ^ (key >> 14);
    key = (key + (key << 2)) + (key << 4); 
    key = key ^ (key >> 28);
    key = key + (key << 31);
#else
    key = ~key + (key << 15); 
    key = key ^ (key >> 12);
    key = key + (key << 2);
    key = key ^ (key >> 4);
    key = key * 2057; 
    key = key ^ (key >> 16);
#endif
    return key;
  }

  static ptrdiff_t x31_string_hash(const char* s) {
    ptrdiff_t h = *s;
    if(h) for(++s; *s; ++s) h = (h << 5) - h + *s;
    return h;
  }

  static ptrdiff_t hash_value(Value* x, bool& unhashable) {
    unhashable = false;
    switch(x->get_type()) {
      case STRING: return x31_string_hash(x->string_data());
      // Why do we hash symbol names instead of symbols directly? Because
      // symbol pointers change during a compaction, and it's either this or 
      // rebuild every hash table that uses symbols as keys after compaction
      case SYMBOL: return x31_string_hash(x->symbol_name()->string_data());
      // For atomic values, identity is value
      case FIXNUM:
      case CONSTANT:
        return wang_integer_hash((ptrdiff_t) x);
      // Unhashable stuff
      default: 
        unhashable = true;
        return 0;
    }
  }

  Value* unhashable_error(Value* value) {
    std::ostringstream ss;
    ss << "unhashable value " << value;
    return make_exception(S_PIP_TABLE, ss.str());
  }

  static ptrdiff_t hash_index(Table* table, Value* key, bool& unhashable) {
    ptrdiff_t hash = hash_value(key, unhashable);
    return hash & (table->chains->length - 1);
  }

  void table_grow(Table* table) {
    // Create new storage
    VectorStorage* old_chains = table->chains;
    Value *chain = 0;
    PIP_S_FRAME(table, old_chains, chain);
    table_setup(table, table->size_log2 + 1);
    // Insert all old values
    for(size_t i = 0; i != old_chains->length; i++) {
      chain = old_chains->data[i];
      while(chain->get_type() == PAIR) {
        table_insert(table, chain->caar(), chain->cdar());
        chain = chain->cdr();
      }
    }
  }

  Value* table_insert(Table* table, Value* key, Value* value) {
    PIP_ASSERT(table->get_type() == TABLE);    

    Value* chain = 0;
    PIP_S_FRAME(table, key, value, chain);

    // Should we grow the hash table?
    if(table->entries >= table->max_entries) {
      table_grow(table);
    }

    bool unhashable;
    ptrdiff_t index = hash_index(table, key, unhashable);
    if(unhashable) return unhashable_error(key);
    
    // Build chain
    chain = cons(key, value);

    // Collision!
    if(table->chains->data[index]) {
      // Prepend this value
      chain = cons(chain, table->chains->data[index]);
    } else {
      // Create list
      chain = cons(chain, PIP_NULL);
    }

    // Insert chain
    table->chains->data[index] = chain;
    table->entries++;
    return PIP_FALSE;
  }

  Value* table_get_cell(Table* table, Value* key) {
    bool unhashable;
    ptrdiff_t index = hash_index(table, key, unhashable);
    if(unhashable) return unhashable_error(key);
    Value* chain = 0;
    chain = table->chains->data[index];
    while(chain->get_type() == PAIR) {
      if(equals(chain->caar(), key)) {
        return chain->car();
      }
      chain = chain->cdr();
    }
    return PIP_FALSE;
  }

  Value* table_set(Table* table, Value* key, Value* value) {
    PIP_ASSERT(table->get_type() == TABLE);
    Value* cell = table_get_cell(table, key);
    if(cell && !cell->active_exception())
      cell->set_cdr(key);
    return cell;
  }

  Value* table_get(Table* table, Value* key, bool& found) {
    PIP_ASSERT(table->get_type() == TABLE);
    Value* cell = table_get_cell(table, key);
    if(cell) {
      found = true;
      return cell->cdr();
    } else {
      found = false;
      return PIP_FALSE;
    }
  }  

  bool table_contains(Table* table, Value* key) {
    PIP_ASSERT(table->get_type() == TABLE);
    return table_get_cell(table, key) != PIP_FALSE;
  }

  ///// (READ) Expression reader and source code tracking 

  // <--
  // Before reading expressions from a source (which may be a file or a string
  // provided by e.g. a program that embeds Pip, or a user entering expressions
  // at the command line), the source is first "registered" with the Pip
  // runtime. It receives a unique numeric identifier and the entire text is
  // read and saved. This is done in order to provide helpful error messages at
  // every stage of program execution.

  // The reader itself is fairly simple. It's split into a tokenizer and a
  // parser; the tokenizer does most of the heavy lifting. The only parsing
  // required is reading lists. The tokenizer is capable of previewing a single
  // token.

  // The reader attaches source code information to lists and symbols. In Pip,
  // lists can be allocated with extra fields for their file and line. Symbols
  // on the other hand are placed in a special "Box" structure, which is
  // unwrapped by the compiler and then becomes garbage. Literal constants,
  // such as #f and 12345, don't cause compilation or runtime errors and
  // therefore don't need to be wrapped.
  // --> 

  // Source code tracking

  // A descriptive name for a source; usually a file path
  std::vector<std::string> source_names;
  // The actual source code, line by line
  std::vector<std::vector<std::string>* > source_contents;
  // File id counter
  unsigned source_counter;

  // Read every line from an input stream
  void read_lines(std::istream& is) {
    std::string line;
    std::vector<std::string>* contents = new std::vector<std::string>();
    while(true) {
      if(!std::getline(is, line)) break;
      contents->push_back(line);
    }
    source_contents.push_back(contents);
  }

  // Load the source code of a file
  // Return 0 on failure
  unsigned register_file(const char* path) {
    std::fstream fs(path, std::fstream::in);
    std::string line;
    if(!fs.is_open()) return 0;
    source_names.push_back(path);
    read_lines(fs);

    return source_counter++;
  }

  // Load the source code of a string
  // "name" is ideally something describing where the string came from
  unsigned register_string(const std::string& name,
                          const std::string& string) {
    source_names.push_back(name);

    std::stringstream ss;
    ss << std::noskipws;
    ss << string;
    read_lines(ss);

    return source_counter++;
  }

  // The actual reader
  struct Reader {
    // Character types

    // TODO: Replace with a table of properties to avoid branching
    // True if a character is a type of quote
    static bool quotep(char c) {
      return c == '\'' || c == '`' || c == ',';
    }
    
    // True if a character is a delimiter; used to determine what is NOT a
    // valid symbol character
    static bool delimiterp(char c) {
      return c == '(' || c == ')' || c == ';' || c == '@' ||
        c == EOF || isspace(c) || c == '.' || c == '#' || c == '"'
        || quotep(c);
    }

    // True if a character is a symbol starting character (all non-delimiters
    // and non-numbers)
    static bool symbol_startp(char c) {
      return (isalpha(c) || ispunct(c)) && !delimiterp(c);
    }

    // True if a character is a symbol character (all symbol characters and
    // numbers)
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

    Reader(State& state_, std::istream& input_, unsigned file_): state(state_),
    input(input_),
    file(file_), line(1), token_value(state), token_lookahead(TK_LOOKAHEAD) {
      
    }
    ~Reader() { }

    State& state;
    std::istream& input;
    // File and current line
    unsigned file, line;
    // Values read in by the tokenizer, or exceptions encountered while
    // tokenizing
    Handle<Value> token_value;
    Token token_lookahead;
    std::string token_buffer;

    // Error handling

    Token lex_error(const std::string& msg) {
      std::ostringstream ss;
      ss << state.source_names[file] << ':' << line << ": " << msg;
      token_value = state.make_exception(State::S_PIP_READ, ss.str());
      return TK_EXCEPTION;
    }

    Value* parse_error(const std::string& msg) {
      std::ostringstream ss;
      ss << state.source_names[file] << ':' << line << ": " << msg;
      return state.make_exception(State::S_PIP_READ, ss.str());
    }

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
            std::cerr << "WARNING: integer overflow while reading fixnum" 
            << std::endl;
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
              PIP_FRAME(s);
              s = state.make_symbol(token_buffer);
              token_value = state.make_box(s, true, file, line);

              return TK_SYMBOL;
            }
          }
        }

        // If it's not a number or a symbol, it will begin with a single
        // character
        switch(c) {
          // Whitespace handling
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
          // Quotes
          case '\'': return TK_QUOTE;
          case '`': return TK_QUASIQUOTE;
          case ',': {
            c = getc();
            if(c == '@') return TK_UNQUOTE_SPLICING;
            ungetc(c);
            return TK_UNQUOTE;
          }
          // Strings
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
          // Read syntax (true and false)
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
      return lex_error("failed to use character");
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
    
    // The list reader
#define PIP_READ_CHECK_TK(x) if((x) == TK_EXCEPTION) return pop_token_value(); 

    Value* read_list(Value* initial = 0) {
      Value *head = 0, *tail = 0, *elt = 0, *swap = 0;
      // Save the line this list begins on
      size_t l = line; 
      PIP_FRAME(initial, head, tail, elt, swap);
      // If we've been given an initial element (say quote in a quoted
      // expression), attach it
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
          std::ostringstream ss;
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

      // Read last element of dotted list -- we also need to make sure it's the
      // last element in the list
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
        if(token != TK_RPAREN)
          return parse_error("more than one element at the end of a dotted"\
                             "list");
        discard_lookahead();
      }

      // Attach source info by re-creating the head of the list with source
      // information attached
      if(head->get_type() == PAIR) {
        swap = head->car();
        elt = head->cdr();
        head = state.cons(swap, elt, true, file, l);
      }

      return head == PIP_FALSE ? PIP_NULL : head;
    }

    // The actual entry point for the reader. Basically a fancy dispatcher for
    // list reading, including shortcuts such as quotes
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
            if(peek_token(false, false) == TK_RPAREN) {
              discard_lookahead(); 
              return PIP_NULL;
            }
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
            PIP_FRAME(cell, cell2, value, symbol);
            value = read();
            PIP_CHECK(value);
            if(value == PIP_EOF)
              return parse_error("unexpected EOF after quote");
            symbol = state.global_symbol(g);
            cell2 = state.cons(value, PIP_NULL);
            cell = state.cons(symbol, cell2, true, file, l);
            return cell;
          }
          case TK_DOT:
            return parse_error("standalone dot encountered in source code "\
                "(dots should only be part of lists)");
          case TK_EOF: return PIP_EOF;
          case TK_LOOKAHEAD: case TK_WHITESPACE:
            return parse_error("parser could not handle token");
          case TK_NEWLINE:
            continue;
        }
      }
    }
  };

  ///// (CC) Compiler

  // <--
  // The Pip compiler is a simple, one-pass compiler. An instance of the
  // compiler is created for each function (for the purposes of the compiler,
  // something like a library source file is considered a function). There is
  // no separate parsing step; the compiler operates directly on s-expressions.
  // It is a little hairy in places because of this, but doing everything in
  // one pass makes it much shorter.

  // The compiler compiles a very small subset of Scheme. Most special forms,
  // such as let, are implemented with macros.

  // Perhaps the most complex machinery in the compiler is the handling of free
  // variables (for Pip's purposes, a "free variable" is defined as any
  // variable that might exist after its function returns; I'm not sure that's
  // the right definition but I'm not changing it now). When a function
  // references, either by setting or getting, any variable from a higher
  // function, the compiler causes that variable to be pulled into every
  // function from that function on down as an "upvalue", which is basically
  // a heap-allocated pointer. All of those functions become Closures, a
  // combination of a function and a set of upvalues. These variables are then
  // set and retreived through the Upvalue structure. This ensures that access
  // to all variables is O(1) and that large structures do not need to be
  // allocated (and left) on the heap to support function calls.
  // -->
  
  // Some structures that are shared by the virtual machine and compiler
  // Virtual machine instructions
  enum {
    OP_BAD = 0,
    OP_PUSH_IMMEDIATE = 1,
    OP_PUSH_CONSTANT = 2,
    OP_POP = 3,
    OP_GLOBAL_SET = 4,
    OP_GLOBAL_GET = 5,
    OP_LOCAL_SET = 6,
    OP_LOCAL_GET = 7,
    OP_UPVALUE_SET = 8,
    OP_UPVALUE_GET = 9,
    OP_CLOSE_OVER = 10,
    OP_RETURN = 11,
    OP_APPLY = 12,
    OP_TAIL_APPLY = 13,
    OP_JUMP_IF_FALSE = 14,
    OP_JUMP = 15,
  };

  struct UpvalLoc {
    UpvalLoc(bool l, unsigned i): local(l), index(i) {}
    bool local;
    unsigned index;
  };

  // Compilation environment format

  // An environment is a pair containing a "parent" environment or a vector of
  // "imported" environments as the car, and a symbol table as the cdr

  // Top-level environment:
  // Either (#f . <table>) 
  // Or (#(<imported env> ...) . <table>)

  // Where entries in <table> are either 'special', a syntax transformer, or a
  // symbol name indicating a global variable

  // Local environment:
  // (<parent env> . <table>)

  // Where entries in <table> are either syntax transformers or a fixnum
  // indicating a local variable's index in the virtual machine's locals array

  Pair* make_env(Value* parent = PIP_FALSE) {
    Table* table = 0;
    PIP_S_FRAME(parent, table);
    table = make_table();
    return cons(parent, table);
  }

  bool env_contains(Pair* env, Value* key) {
    return table_contains(PIP_CAST(Table, env->cdr), key);
  }

  void env_define(Pair* env, Value* key, Value* value) {
    Table* table = PIP_CAST(Table, env->cdr);
    Value* chk ;
    if(env_contains(env, key)) {
      chk = table_set(table, key, value);
      std::cerr << "WARNING: redefining " << key << std::endl;
    } else {
      chk = table_insert(table, key, value);
    }
    PIP_ASSERT(!chk->active_exception());
  }

  // If an environment has no parent environment, or a list of delegate
  // environment, its variables will be global variables
  bool env_globalp(Pair* env) {
    return env->car == PIP_FALSE || env->car->get_type_unsafe() == VECTOR;
  }

  // Look up an environment variable (the result is rather complex, and
  // returned in a structure)

  enum RefScope { REF_GLOBAL, REF_UP, REF_LOCAL };
  struct Lookup {
    Lookup(): scope(REF_LOCAL), success(true) {
      // Make sure everything is zero-initialized
      nonglobal.index = nonglobal.level = 0;
      global = nonglobal.value = 0;
    }

    RefScope scope;
    bool success;
    union {
      Value* global;
      struct { unsigned index, level; Value* value; } nonglobal;
    };
  };

  // Determines whether a lookup result refers to a special form or macro
  bool lookup_syntaxp(Lookup& lookup) {
    if(lookup.success) {
      if(lookup.scope == REF_GLOBAL) {
        return lookup.global == global_symbol(S_SPECIAL) ||
               lookup.global->applicablep();
      } else {
        return lookup.nonglobal.value->applicablep();
      }
    }
    return false;
  }

  void env_lookup(Pair* env, Value* key, Lookup& lookup) {
    Pair* start_env = env;
    // Search through environments
    while(env->get_type() == PAIR) {
      Table* table = PIP_CAST(Table, env->cdr);
      // Search through an environment
      Value* cell = table_get_cell(table, key);
      if(cell != PIP_FALSE) {
        // Found a match -- determine scope
        PIP_ASSERT(cell->get_type() == PAIR);
        if(env_globalp(env)) {
          lookup.scope = REF_GLOBAL;
          lookup.global = cell->cdr();
          return;
        } else {
          // If we're still searching through the original environment, it's a
          // local variable, and if not, it's a free variable
          lookup.scope = env == start_env ? REF_LOCAL : REF_UP;
          lookup.nonglobal.value = cell->cdr();
          // If this is a variable and not a macro, calculate its local index
          if(cell->cdr()->get_type() == FIXNUM) {
            lookup.nonglobal.index = cell->cdr()->fixnum_value();
          }
          return;
        }
      }
      // No dice (note that even though this is in a union with global, it's
      // fine to increment because it'll be discarded if we go all the way to a
      // global variable)
      lookup.nonglobal.level++;
      // TODO: Delegates
      env = static_cast<Pair*>(env->car);
    }
    lookup.success = false;
  }

  // Search for a variable. Note that the return is convoluted and should
  // probably be re-written for clarity

  // The direct return value will be either a variable name (in the case of a
  // global variable), 'variable in the case of a local/free variable, or #f in
  // the case of a failed lookup

  Handle<Pair> core_env;

  // A structure describing the location of a free variable relative to the
  // function it's being used in
  struct FreeVariableInfo {
    // The lexical location of the variable
    unsigned lexical_level, lexical_index;
    // The upvalue's index at runtime, either in the upvalues array of the
    // preceding function, or in the locals array of the currently executing
    // function
    unsigned index;
  };

  // An instance of the compiler, created for each function
  struct Compiler {
    Compiler(State& state_, Compiler* parent_, Pair* env_ = NULL,
             size_t depth_ = 0): 
      state(state_), parent(parent_),
      local_free_variable_count(0), upvalue_count(0),
      stack_max(0), stack_size(0), locals(0), constants(state), closure(false),
      name(state_), env(0), depth(depth_), last_insn(0) {
      if(!env_) env = &state.core_env;
      else env = new Handle<Pair>(state, env_);
      env_globalp = state.env_globalp(**env);
    }
    ~Compiler() {
      if(env != &state.core_env) delete env;    
    }

    State& state;
    // The parent function -- necessary for free variables
    Compiler* parent;
    // The wordcode
    std::vector<size_t> code;
    // A vector containing combinations of instruction offsets and source
    // location info for debugging and tracebacks
    std::vector<std::pair<unsigned, SourceInfo> > debuginfo;
    // A vector containing free variable locations
    std::vector<FreeVariableInfo> free_variables;
    unsigned local_free_variable_count, upvalue_count;

    unsigned stack_max, stack_size, locals;
    Handle<Vector> constants;
    bool closure;
    // The name of the function (if any)
    Handle<String> name;
    // The environment of the function
    bool env_globalp;
    Handle<Pair>* env;
    // The depth of the function (for debug message purposes)
    size_t depth;
    // Location of last instruction emitted (for debug message purposes)
    size_t last_insn;

    // Location of last applications emitted, which will be replaced with tail
    // calls 
    size_t last_apply1, last_apply2;

    // Code generation
    void emit(size_t word) {
      last_insn = code.size();
      code.push_back(word);
    }

    void emit_arg(size_t word) {
      code.push_back(word);
    }

    // In order to push a constant onto the stack, we have to save it and store
    // it along with the function. This function lazily creates the vector of
    // constants and makes sure no constants are repeated.
    void push_constant(Value* constant) {
      push();
      Vector* cs = *constants;
      PIP_FRAME(cs, constant);
      // Lazily create constant vector
      if(cs == PIP_FALSE) {
        cs = state.make_vector();
        constants = cs;
      }
      // Check for existing constant
      for(unsigned i = 0; i != cs->vector_length(); i++) {
        if(cs->vector_ref(i) == constant) {
          // Constant already exists, don't append it to vector
          emit(OP_PUSH_CONSTANT);
          emit_arg(i);
          PIP_CC_VMSG("push-constant " << constant << " " << i <<
                      " (existing constant)");
          return;
        }
      }
      // Constant does not exist, add to vector and then push
      unsigned i = state.vector_append(cs, constant);
      emit(OP_PUSH_CONSTANT);
      emit_arg(i);
      PIP_CC_EMIT("push-constant " << constant << " " << i);
    }

    // Stack size management; determines how much memory will be allocated for
    // the stack
    void push(ptrdiff_t i = 1) {
      stack_size += i;
      if(stack_size > stack_max) stack_max = stack_size;
      PIP_CC_VMSG("PUSH: stack_size = " << stack_size);
    }

    void pop(ptrdiff_t i = 1) {
      stack_size -= i;
      PIP_ASSERT(stack_size >= 0);
      PIP_CC_VMSG("POP: stack_size = " << stack_size);
    }

    // Dealing with symbols which are boxed and contain source code location
    // info    
    Value* unbox(Value* x) {
      if(x->get_type() == BOX) return x->box_value();
      return x;
    }

    // Annotating virtual machine instructions with their source location for
    // debug purposes
    void annotate(Value* exp) {
      SourceInfo src;
      unwrap_source_info(exp, src);
      if(src.file) {
        unsigned insn = code.size() - 1;
        std::pair<unsigned, SourceInfo> dbg(insn, src);
        debuginfo.push_back(dbg);
      }
    }

    // error handling
    Value* syntax_error(Value* form, const std::string& str) {
      std::string out;
      state.format_source_error(form, str, out);
      return state.make_exception(State::S_PIP_COMPILE, out);
    }

    Value* arity_error(Global name, Value* form, size_t expected, unsigned got) {
      std::ostringstream ss;
      ss << "malformed " << state.global_symbol(name) << ": expected "
         << expected << " arguments, but got " << got;
      return syntax_error(form, ss.str());
    }

    Value* arity_error_most(Global name, Value* form, size_t expected,
                            unsigned got) {
      std::ostringstream ss;
      ss << "malformed " << state.global_symbol(name) << ": expected at most "
         << expected << " arguments, but got " << got;
      return syntax_error(form, ss.str());
    }

    Value* syntax_error(Global name, Value* form, const std::string& str) {
      std::ostringstream ss;
      ss << "malformed " << state.global_symbol(name) << ": " << str;
      return syntax_error(form, ss.str());
    }

    Value* undefined_variable(Value* form) {
      std::ostringstream ss;
      ss << "reference to undefined variable '" << form << '\'';;
      std::string out;
      state.format_source_error(form, ss.str(), out);
      return state.make_exception(State::S_PIP_COMPILE, out);
    }

    // free variable handling
    unsigned register_free_variable(Lookup lookup) {
      FreeVariableInfo l;
      // Check for existing free variable
      for(unsigned i = 0; i != free_variables.size(); i++) {
        l = free_variables[i];
        if(l.lexical_level == lookup.nonglobal.level &&
           l.lexical_index == lookup.nonglobal.index)
          return i;
      }
      // New free variable - register it with this and all preceding functions
      // as necessary. 
      l.lexical_level = lookup.nonglobal.level;
      l.lexical_index = lookup.nonglobal.index;
      PIP_CC_VMSG("register free variable <" << l.lexical_level << ", "
                  << l.lexical_index << '>');
      Lookup lookup_copy(lookup);
      lookup_copy.nonglobal.level--;
      if(lookup.nonglobal.level != 0) {
        // Register with preceding function, and make this a closure if it
        // isn't already
        closure = true;
        l.index = parent->register_free_variable(lookup_copy);
        upvalue_count++;
      } else {
        // If the "level" is 0, it's actually a local variable, so we
        // don't need necessarily need to make this function a closure
        l.index = l.lexical_index;
        local_free_variable_count++;
      }
      free_variables.push_back(l);
      return free_variables.size() - 1;
    }

    // Compile a variable reference
    Value* compile_ref(Value* exp) {
      annotate(exp);
      Value* name = unbox(exp);
      Lookup lookup;
      state.env_lookup(**env, name, lookup);
      if(!lookup.success) return undefined_variable(exp);
      switch(lookup.scope) {
        case REF_GLOBAL:
          if(lookup.global == state.global_symbol(S_SPECIAL))
            return syntax_error(exp, "special forms cannot be used as values");
          if(lookup.global->applicablep())
            return syntax_error(exp, "macros cannot be used as values");
          // Why do we need lookup.global when we already have the name?
          // Because it could be a reference to a module variable, 
          // eg car => #core.car
          push_constant(lookup.global);
          emit(OP_GLOBAL_GET);
          PIP_CC_EMIT("global-get " << name);
          // No stack change -- the name will be popped, but the value will be
          // pushed       
          break;
        case REF_UP:
        case REF_LOCAL: {
          // Make sure we haven't been passed a macro (there can be local
          // macros in the case of let-syntax)
          if(lookup.nonglobal.value->applicablep()) 
            return syntax_error(exp, "macros cannot be used as values");
          if(lookup.scope == REF_LOCAL) {
            emit(OP_LOCAL_GET);
            emit_arg(lookup.nonglobal.index);
            PIP_CC_EMIT("local-get " << name << ' ' << lookup.nonglobal.index);
            push();
          } else {
            PIP_ASSERT(lookup.scope == REF_UP);
            unsigned index = register_free_variable(lookup);
            emit(OP_UPVALUE_GET);
            emit_arg(index);
            PIP_CC_EMIT("upvalue-get " << name << ' ' << index);
            push();
          }
          break;
        }
      }
      return PIP_FALSE;
    }

    // Generate a set! (used by both define and set!)
    // Assumes value is already on the stack
    void generate_set(Lookup& lookup, Value* name) {
      switch(lookup.scope) {
        case REF_GLOBAL:
          push_constant(name);
          emit(OP_GLOBAL_SET);
          PIP_CC_EMIT("global-set " << name);
          pop(2);
          break;
        case REF_LOCAL:
          emit(OP_LOCAL_SET);
          emit_arg(lookup.nonglobal.index);
          PIP_CC_EMIT("local-set " << (unsigned) lookup.nonglobal.index);
          pop();
          break;
        case REF_UP: assert(0);
      }
    }

    // Special forms
    Value* compile_define(size_t argc, Value* exp, bool tail) {
      // There's a goto here (yuck) because we'll restart the compilation
      // process if this define is a function definition
restart:
      Value* chk = 0;
      if(argc != 2) return arity_error(S_DEFINE, exp, 2, argc);
      Value* name = unbox(exp->cadr());
      Value* args = 0;
      if(name->get_type() != SYMBOL) {
        // Parse a lambda shortcut, e.g. (define (x) #t) becomes 
        // (define x (#named-lambda x () #t))
        if(name->get_type() == PAIR) {
          args = name->cdr();
          name = name->car();
          if(unbox(name)->get_type() != SYMBOL)
            return syntax_error(S_DEFINE, exp, "first argument to define a "
                                "function must be a symbol");
          Value* named_lambda = 0, *tmp = 0;
          PIP_FRAME(exp, name, args, named_lambda, tmp);
          // Get the argument list and body
          tmp = state.cons(args, exp->cddr());
          // Put the function's name in front of them
          tmp = state.cons(name, tmp);
          // Finally put all that in a (named-lambda ...) expression
          named_lambda = state.cons(state.global_symbol(S_NAMED_LAMBDA), tmp);
          named_lambda = state.cons_source(named_lambda, PIP_NULL, exp);
          // This is kind of hacky, but we're just going to twiddle the
          // expression a little bit and start parsing again
          exp->cdr()->set_car(name);
          exp->cdr()->set_cdr(named_lambda);
          goto restart;
        } else return syntax_error(S_DEFINE, exp,
                                  "first argument to define must be a symbol");
      }
      // Parse a define lambda expression, e.g.: 
      // (define name (lambda () #t))
      // becomes
      // (define name (#named-lambda name () #t)
      // TODO: Handle (define name (lambda () #t))

      // We'll create a fake lookup structure for generate_set here
      Lookup lookup;
      Value* value = 0;
      if(env_globalp) {
        // If this is a global variable, the compile-time value of the variable
        // will be the actual global symbol
        lookup.scope = REF_GLOBAL;
        value = name;
      } else {
        // If this is not a global variable, we need to register it as a local
        // The compile-time value of the variable will be a fixnum index
        lookup.scope = REF_LOCAL;
        lookup.nonglobal.index = locals;
        value = Value::make_fixnum(locals++);
      }

      // TODO: Allow re-definitions, but warn about them
      state.env_define(**env, name, value);

      // Compile body for set!
      PIP_FRAME(name);
      chk = compile(exp->cddr()->car());
      PIP_CHECK(chk);
      // Set variable
      generate_set(lookup, name);
      return PIP_FALSE;
    }

    Value* compile_set(size_t argc, Value* exp, bool tail) {
      Value* chk = 0;
      // set always has two arguments
      if(argc != 2) return arity_error(S_SET, exp, 2, argc);
      Value* name = unbox(exp->cadr());
      // Check that name is a symbol
      if(name->get_type() != SYMBOL)
        return syntax_error(S_SET, exp,
                            "first argument to set! must be a symbol");

      // Compile body 
      chk = compile(exp->cddr()->car());
      PIP_CHECK(chk);

      // Look up variable
      Lookup lookup;
      state.env_lookup(**env, name, lookup);
      // Don't allow special forms or syntax
      if(!lookup.success)
        return undefined_variable(exp->cadr());
      if(state.lookup_syntaxp(lookup))
        return syntax_error(exp, "syntax cannot be set! like a variable");

      generate_set(lookup, name);
      return PIP_FALSE;
    }

    Value* compile_begin(size_t argc, Value* exp, bool tail) {
      if(argc == 0) {
        return compile(PIP_UNSPECIFIED, tail);
      } else {
        Value* body = exp->cdr();
        Value* elt = 0, *chk = 0;
        PIP_FRAME(body, elt, chk);
        while(body != PIP_NULL) {
          elt = body->car();
          if(body->cdr() == PIP_NULL) {
            return compile(elt, tail);
          } else {
            chk = compile(elt, false);
            PIP_CHECK(chk);
            body = body->cdr();
            // Pop result
            emit(OP_POP);
            pop();
          }
        }
      }
      return PIP_FALSE; // should never be reached
    }

    Value* compile_lambda(size_t form_argc, Value* exp, bool tail,
                          Value* name) {
      // For checking subcompilation results
      Value* chk = 0;
      if(form_argc < 2) return arity_error(S_LAMBDA, exp, 2, form_argc);
      // Create new compiler, environment
      unsigned argc = 0;
      // Will be set to true if the function is variable arity
      // (i.e. takes unlimited arguments)
      bool variable_arity = false;
      Pair *new_env = 0;
      Value *args = 0, *body = 0;
      Prototype* proto = 0;
      PIP_FRAME(exp, new_env, args, body, proto);
      // Create a new environment for the function with the current environment
      // as its parent
      new_env = state.make_env(**env);
      // Make the current environment its parent environment
      // state.vector_append(PIP_CAST(Vector, new_env), **env);
      // Now parse the function's arguments and define them within the
      // new environment
      args = exp->cadr();
      if(args != PIP_NULL) {
        if(args->get_type() != PAIR) 
          return syntax_error(S_LAMBDA, exp, "first argument to lambda must be"
                              "either () or a list of arguments");
        while(args->get_type() == PAIR) {
          Value* arg = unbox(args->car());
          if(arg->get_type() != SYMBOL) {
            return syntax_error(S_LAMBDA, exp,
                                "lambda argument list must be symbols");
          }
          // Finally, we can add them to the environment!
          // Add it as a local variable
          state.env_define(new_env, arg, Value::make_fixnum(argc));
          argc++;
          args = args->cdr();
          if(args->get_type() != PAIR) {
            variable_arity = true;
            break;
          }
        }
      }
      // Compile the function's body
      PIP_CC_MSG("compiling subfunction");
      Compiler cc(state, this, new_env, depth+1);
      cc.locals = argc;
      cc.name = static_cast<String*>(name);
      body = exp->cddr();
      while(body != PIP_NULL) {
        // Compile body expression. Note that if cdr() is PIP_NULL, this is the
        // tail expression and might be subject to tail call optimization
        chk = cc.compile(body->car(), body->cdr() == PIP_NULL);
        PIP_CHECK(chk);
        body = body->cdr();
      }
      proto = cc.end(argc, variable_arity);
      push_constant(proto);
      // If the function's a closure, we'll have to close over it
      if(cc.closure) {
        PIP_CC_EMIT("close-over");
        emit(OP_CLOSE_OVER);
      }
      return PIP_FALSE;
    }

    Value* compile_if(size_t argc, Value* exp, bool tail) {
      if(argc != 2 && argc != 3) {
        if(argc < 2) return arity_error(S_IF, exp, 2, argc);
        else return arity_error_most(S_IF, exp, 3, argc);
      }
      Value* chk = 0, *condition = 0, *then_branch = 0, *else_branch = 0;
      PIP_FRAME(chk, condition, then_branch, else_branch);

      condition = exp->cadr();
      then_branch = exp->cddr()->car();
      else_branch = argc == 2 ? PIP_UNSPECIFIED : exp->cddr()->cdr()->car();

      chk = compile(condition);
      PIP_CHECK(chk);
      // Jump to the else branch if the condition is false
      emit(OP_JUMP_IF_FALSE);
      // Emit placeholder for argument
      size_t jmp1 = code.size();
      emit_arg(0);
      // JMP_IF_FALSE will pop condition
      pop();
      PIP_CC_EMIT("jump-if-false");
      size_t old_stack = stack_size;
      // Emit 'then' code
      chk = compile(then_branch, tail);
      PIP_CHECK(chk);
      pop(stack_size - old_stack);
      // Jmp to the rest of the program
      emit(OP_JUMP);
      // Emit placeholder for argument
      size_t jmp2 = code.size();
      emit_arg(0);
      PIP_CC_EMIT("jump");
      // Location of first jump (beginning of else branch)
      size_t lbl1 = code.size();
      chk = compile(else_branch, tail);
      PIP_CHECK(chk);
      pop(stack_size - old_stack);
      // Location of second jump
      size_t lbl2 = code.size();

      // Replace jumps
      code[jmp1] = lbl1;
      code[jmp2] = lbl2;

      // Ensure space for result
      push();

      PIP_CC_VMSG("if jmp1 " << lbl1 << " jmp2 " << lbl2);

      return PIP_FALSE;
    }

    Value* compile_quote(size_t argc, Value* exp) {
      if(argc != 1) return arity_error(S_QUOTE, exp, 1, argc);
      // Immediates are stored directly in code
      if(exp->cadr()->immediatep()) {
        return compile(exp->cadr());
      } else {
        // Everything else is a constant
        push_constant(exp->cadr());
        return PIP_FALSE;
      }
    }

    // Compile a normal function application
    Value* compile_apply(Value* exp, bool tail) {
      Value* chk = 0;
      // First, compile the arguments. We'll determine how many arguments
      // were passed by comparing the stack size before and after
      // compiling them.
      size_t old_stack_size = stack_size;

      Value* args = exp->cdr();
      while(args != PIP_NULL) {
        chk = compile(args->car());
        PIP_CHECK(chk);
        args = args->cdr();
      }

      size_t argc = stack_size - old_stack_size;
      // Compile the actual function, which might be a symbol or perhaps
      // an inline anonymous function (e.g. ((lambda () #t)))
      chk = compile(exp->car());
      PIP_CHECK(chk);

      emit(tail ? OP_TAIL_APPLY : OP_APPLY);
      PIP_ASSERT(argc < 255);
      emit_arg((unsigned char) argc); 
      PIP_CC_EMIT((tail ? "tail-apply" : "apply") << ' ' << argc);
      // The application will pop the arguments and the function once finished,
      // but will push a result
      pop(stack_size - old_stack_size);
      push();
      return PIP_FALSE;
    }

    Value* compile_define_syntax(size_t argc, Value* exp, bool tail) {
      return PIP_FALSE;
    }

    // Compile a single expression, returns #f or an error if one occurred
    Value* compile(Value* exp, bool tail = false) {
      // Dispatch based on an object's type
      switch(exp->get_type()) {
        // Constants: note that these are only immediate constants, such as #t
        // and #f, and not complex values that the compiler considers
        // constants, such as '(list)
        case FIXNUM:
        case CONSTANT:
          emit(OP_PUSH_IMMEDIATE);
          emit_arg((ptrdiff_t) exp);
          push();
          PIP_CC_EMIT("push-immediate " << exp);
          break;
        // Variable references
        case SYMBOL:
        case BOX: {
          return compile_ref(exp);
          break;
        }
        case PAIR: {
          // Compile applications, including special forms, macros, and normal
          // function applications
          if(!listp(exp))
            return syntax_error(exp, "dotted lists not allowed in source");
          // Note the source location of this application
          annotate(exp);
          // Check for special forms and macros
          Lookup lookup;
          Value* function = unbox(exp->car());
          bool syntax = false;
          if(function->get_type() == SYMBOL) {
            state.env_lookup(**env, function, lookup);
            syntax = state.lookup_syntaxp(lookup);
          }
          if(syntax) {
            if(lookup.global == state.global_symbol(S_SPECIAL)) {
              // Dispatch on the special form
              Symbol* op = PIP_CAST(Symbol, function);
              size_t argc = length(exp->cdr());
              // define
              if(op == state.global_symbol(S_DEFINE)) {
                return compile_define(argc, exp, tail);
              // set!
              } else if(op == state.global_symbol(S_SET)) {
                return compile_set(argc, exp, tail);
              // begin
              } else if(op == state.global_symbol(S_BEGIN)) {
                return compile_begin(argc, exp, tail);
              // lambda
              } else if(op == state.global_symbol(S_LAMBDA)) {
                return compile_lambda(argc, exp, tail, PIP_FALSE);
              // named-lambda
              } else if(op == state.global_symbol(S_NAMED_LAMBDA)) {  
                // This is a little hacky, but we're going to extract the name
                // here and then make it look like a normal lambda for
                // compile_lambda
                
                // No need for parsing because named lambdas are generated by the
                // parser
                Value* name = exp->cadr();
                Value* rest = exp->cddr();
                exp->set_cdr(rest);

                return compile_lambda(argc-1, exp, tail, name);
              } else if(op == state.global_symbol(S_IF)) {
                return compile_if(argc, exp, tail);
              } else if(op == state.global_symbol(S_QUOTE)) {
                return compile_quote(argc, exp);
              } else if(op == state.global_symbol(S_DEFINE_SYNTAX)) {
                return compile_define_syntax(argc, exp, tail);
              } else assert(0);
            } else {
              // Syntax
              PIP_FAIL0("syntax not supported");
            }
          } else {
            // function application
            return compile_apply(exp, tail);
          }
          PIP_ASSERT(0);
        }
        default:
          PIP_ASSERT(!"unknown expression given to compiler");
          break;
      }
      return PIP_FALSE;
    }

    // Creates the actual Prototype at the end of compilation
    Prototype* end(unsigned arguments = 0, bool variable_arity = false) {
      PIP_ASSERT(stack_size == 1);
      emit(OP_RETURN);
      PIP_CC_EMIT("return");
      Prototype* p = 0;

      Blob* codeblob = 0, *localfreevars = 0, *upvals = 0;
      PIP_FRAME(p, codeblob, localfreevars, upvals);
      p = state.allocate<Prototype>();
      // Copy some data from the compiler to Scheme blobs
      codeblob = state.make_blob<size_t>(code.size());
      memcpy(codeblob->data, &code[0], code.size() * sizeof(size_t));
      p->code = codeblob;

      // Free variable handling
      localfreevars = state.make_blob<unsigned>(local_free_variable_count);
      upvals = state.make_blob<UpvalLoc>(upvalue_count);
      size_t freevar_i = 0, upvalue_i = 0;
      for(size_t i = 0; i != free_variables.size(); i++) {
        if(free_variables[i].lexical_level == 0)
          localfreevars->blob_set<unsigned>(freevar_i++,
                                            free_variables[i].index);
        else {
          // Note whether the variable is local to the function that
          // will be creating the closure
          UpvalLoc l(free_variables[i].lexical_level == 1,
                     free_variables[i].index);
          upvals->blob_set(upvalue_i++, l);
        }
      }

      p->local_free_variables = localfreevars;
      p->local_free_variable_count = local_free_variable_count;
    
      p->upvalues = upvals;

      p->stack_max = stack_max;
      p->locals = locals;
      // TODO: Compress vector
      p->constants = *constants;
      p->arguments = arguments;
      if(variable_arity)
        p->set_header_bit(PROTOTYPE, Value::PROTOTYPE_VARIABLE_ARITY_BIT);
      return p;
    }
  };

  ///// (VM) VIRTUAL MACHINE

  // - Closes over upvalues so they can still be accessed after this function's
  // frame is gone

  // - Pops the function's frame
#define VM_POP_FRAME() \
  for(size_t i = 0; i != prototype->local_free_variable_count; i++) { \
    f.upvalues[i]->upvalue_close(); \
  } \
  vm_frames.pop_back(); \
  vm_depth--; 
  // Return that properly unwinds the stack

#define VM_RETURN(x) { \
  VM_POP_FRAME(); \
  Value* res = (x); \
  return res; }

  // Check for exceptions and unwind the stack if necessary
#define VM_CHECK(x) \
  if(x->active_exception()) { \
    VM_RETURN(x); \
  }

  size_t vm_depth;
  // Passing arguments for tail calls. No need to protect 
  // for garbage collector because no allocations will be triggered while
  // trampolining.
  std::vector<Value*> vm_trampoline_arguments;
  Value* vm_trampoline_function;

  // A frame containing information that needs to be tracked by the garbage
  // collector
  struct VMFrame {
    VMFrame(Prototype* p_, Closure* c_, Value** stack_, Value** locals_):
      p(p_), c(c_), stack(stack_), locals(locals_), upvalues(0), si(0)
      {}
    // Need to save pointers to the prototype and closure so they're considered
    // roots
    Prototype* p;
    Closure* c;
    // Storage for stack and locals
    Value** stack;
    Value** locals;
    Upvalue** upvalues;
    // Stack index
    unsigned si;
  };

  // Evaluation errors
  Value* arity_error() {
    return make_exception(State::S_PIP_EVAL, "function got bad amount of arguments");
  }

  Value* bad_application() {
    return make_exception(State::S_PIP_EVAL, "attempt to apply non-function");
  }

  Value* type_error(const std::string& name, size_t arg_number,
                    Value* argument, Type expected, Type got) {
    std::ostringstream out;
    out << name << " expected argument " << arg_number+1 << " to be " << 
      expected << " but got " << got << " (" << argument << ')';
    return make_exception(State::S_PIP_EVAL, out.str());
  }

  // Apply Scheme function
  Value* vm_apply(Prototype* prototype, Closure* closure, size_t argc,
                  Value* args[], bool& trampoline) {
    // Track how many frames have been lost due to tail calls for debugging
    // purposes eg if we're in a loop and an error occurs on the 857th
    // iteration, we'll be able to print that information
    size_t frames_lost = 0;
    // Track virtual machine depth
    vm_depth++;

    PIP_VM_MSG("entering function ");
    // Get pointer to code
    // NOTE: could be moved if moving garbage collector is ever used
    size_t* wc = (size_t*)(prototype->code->data);

    // Set up call frame
    VMFrame f(prototype, closure, 
        static_cast<Value**>(alloca(prototype->stack_max * sizeof(Value*))),
        static_cast<Value**>(alloca(prototype->locals * sizeof(Value*))));
    // Clear local storage (necessary because garbage collector might look at
    // them even before they are set)
    memset(f.locals, 0, prototype->locals * sizeof(Value*));
    // Push call frame
    vm_frames.push_back(&f);

    // Create upvalues, if necessary
    if(prototype->local_free_variable_count) {
      unsigned* data = (unsigned*) prototype->local_free_variables->data;
      
      f.upvalues = (Upvalue**) alloca(prototype->local_free_variable_count * sizeof(Upvalue*));
      memset(f.upvalues, 0, prototype->local_free_variable_count * sizeof(Upvalue*));

      for(size_t i = 0; i != prototype->local_free_variable_count; i++) {
        unsigned index = data[i];
        f.upvalues[i] = allocate<Upvalue>();
        f.upvalues[i]->local = &f.locals[index];
      }
    }

    // Load arguments

    // Check argument count
    if(argc < prototype->arguments) {
      VM_RETURN(arity_error());
    }

    // Load normal arguments
    size_t i;
    for(i = 0; i != prototype->arguments; i++) {
      f.locals[i] = args[i];
    }

    // If we're not done yet, either we've received too many arguments or this
    // is a variable arity function
    if(i != argc) {
      if(prototype->prototype_variable_arity()) {
        return make_exception(State::S_PIP_EVAL, "variable arity not supported yet");
      } else {
        VM_RETURN(arity_error());
      }
    }

    // Some common things you will see:
    // f.stack[f.si-1] Get the top of the stack 
    // f.stack[--f.si] Pop the top of the stack
    // wc[ip++] Get the next instruction and advance the instruction pointer

    // Evaluate function
    unsigned ip = 0;
    while(1) {
      switch(wc[ip++]) {
        case OP_PUSH_IMMEDIATE:
          f.stack[f.si++] = (Value*) wc[ip++]; 
          continue;
        case OP_PUSH_CONSTANT:
          f.stack[f.si++] = f.p->constants->vector_ref(wc[ip++]);
          continue;
        case OP_POP:
          --f.si;
          continue;
        case OP_GLOBAL_SET: {
          // Get the global name
          Symbol* k = PIP_CAST(Symbol, f.stack[f.si-1]);
          --f.si;
          // Pop the value off the stack
          Value* v = f.stack[--f.si];
          PIP_VM_VMSG("global-set " << k << ' ' << v);
          // Set the value
          k->value = v;
          continue;
        }
        case OP_GLOBAL_GET: {
          // Pop the global name (this is guaranteed to be a symbol)
          Symbol* k = PIP_CAST(Symbol, f.stack[f.si-1]);
          f.si--;
          // TODO: error messages
          f.stack[f.si++] = k->value;
          continue;
        }
        case OP_LOCAL_SET: {
          // Get the local's index
          size_t index = wc[ip++];
          Value* val = f.stack[--f.si];
          PIP_VM_VMSG("local-set " << (unsigned) index << " " << val);
          f.locals[index] = val;
          continue;
        }
        case OP_LOCAL_GET: {
          // Get the local index
          size_t index = wc[ip++];
          PIP_VM_VMSG("local-get " << (unsigned)index);
          f.stack[f.si++] = f.locals[index];
          continue;
        }
        case OP_UPVALUE_GET: {
          // Get upvalue index
          size_t index = wc[ip++];
          PIP_VM_VMSG("upvalue-get " << (unsigned)index);
          f.stack[f.si++] = closure->upvalues->data[index]->upvalue();
          continue;
        }
        case OP_UPVALUE_SET: {
          // Get upvalue index
          size_t index = wc[ip++];
          Value* val = f.stack[--f.si];
          PIP_VM_VMSG("upvalue-set " << (unsigned)index);
          closure->upvalues->data[index]->upvalue_set(val);
        }
        case OP_CLOSE_OVER: {
          // Create a closure
          PIP_VM_VMSG("close-over");
          Closure* c = 0;
          Prototype* p = PIP_CAST(Prototype, f.stack[f.si-1]);
          Blob* locations = p->upvalues;
          Vector* upvalues = 0;
          PIP_S_FRAME(c, p, locations, upvalues);
          // Allocate the closure
          c = allocate<Closure>();
          c->prototype = p;
          // Determine how many upvalues we have
          size_t locs = locations->blob_length<UpvalLoc>();
          // Allocate the vector that will actually hold the upvalues
          upvalues = make_vector(locs);
          // Fill array
          for(size_t i = 0; i != locs; i++) {
            UpvalLoc l = locations->blob_ref<UpvalLoc>(i);
            if(l.local)
              vector_append(upvalues, f.upvalues[l.index]);
            else
              vector_append(upvalues, closure->upvalues->data[l.index]);
          }
          c->upvalues = upvalues->storage;
          // Replace prototype with closure
          f.stack[f.si-1] = c;
          continue;
        }
        case OP_RETURN: {
          PIP_VM_VMSG("return");
          // Get return value
          Value* ret = f.si ? f.stack[--f.si] : PIP_UNSPECIFIED;
          VM_RETURN(ret);
        }
        case OP_TAIL_APPLY: {
          frames_lost++;
          // Retrieve argument count
          size_t argc = wc[ip++];
          PIP_VM_VMSG("tail-apply " << argc);
          // Pop the function, which should be at the top of the stack
          Value* fn = f.stack[f.si-1];
          f.si--;
          vm_trampoline_arguments.clear();
          for(size_t i = 0; i != argc; i++) {
            vm_trampoline_arguments.push_back(f.stack[f.si-argc+i]);
          }
          vm_trampoline_function = fn;
          trampoline = true;
          VM_RETURN(PIP_UNSPECIFIED);
          continue;
        }
        case OP_APPLY: {
          // Retrieve argument count
          size_t argc = wc[ip++];
          PIP_VM_VMSG("apply " << (unsigned)argc);
          // Pop the function, which should be at the top of the stack
          Value* fn = f.stack[f.si-1];
          f.si--;
          // Apply the function to the arguments by slicing the stack
          Value* tmp = apply(fn, argc, &f.stack[f.si-argc]);
          VM_CHECK(tmp);
          // Pop the rest of the arguments off the stack
          f.si -= argc;
          // Add the result of application to the stack
          f.stack[f.si++] = tmp;
          PIP_VM_VMSG("apply result: " << tmp);
          continue;
        }
        case OP_JUMP_IF_FALSE: {
          size_t insn = wc[ip++];
          PIP_VM_VMSG("jump-if-false "
                      << (f.stack[f.si-1] == PIP_FALSE ? "jmping" :
                          "not jmping") 
                      << " " << insn);
          Value* condition = f.stack[--f.si];
          if(condition == PIP_FALSE) {
            ip = insn;
          } 
          continue;
        }
        case OP_JUMP: {
          PIP_VM_VMSG("jump " << wc[ip]);
          ip = wc[ip];
          continue;
        }
        default: {
          std::cout << (ptrdiff_t)wc[ip-1] << std::endl;
          assert(!"bad instruction");
        }
      }
    }
  }

  // Apply frontend
  Value* apply(Value* function, size_t argc, Value* args[]) {
    size_t frames_lost = 0;
tail:
    bool trampoline = false;
    // Kind of confusingly, this may not be a prototype, but we'll consider it
    // one here to allow code re-use
    Prototype* prototype = static_cast<Prototype*>(function);
    Closure* closure = 0;
    Value* result = 0;
    switch(function->get_type()) {
      case CLOSURE: 
        // If it's a closure, extract the prototype and then the closure
        prototype = static_cast<Closure*>(function)->prototype;
        closure = static_cast<Closure*>(function);
      case PROTOTYPE:
        // Fall-through
        result = vm_apply(prototype, closure, argc, args, trampoline);
        if(trampoline && !result->active_exception()) {
          function = vm_trampoline_function;
          argc = vm_trampoline_arguments.size();
          args = &vm_trampoline_arguments[0];
          frames_lost++;
          goto tail;
        } else return result;
      case NATIVE_FUNCTION: {
        return static_cast<NativeFunction*>(function)->pointer(*this, argc,
                                                               args);
      }
      default:
        PIP_FAIL0("attempt to apply bad type" << function->get_type());
    }
    return PIP_UNSPECIFIED;
  } 

#undef VM_RETURN
#undef VM_CHECK
}; // State

///// GARBAGE COLLECTOR TRACKING IMPLEMENTATION

template <class T>
void Handle<T>::initialize() {
  previous = state.handle_list;
  if(previous)
    previous->next = (Handle<Value> *) this;
  state.handle_list = (Handle<Value> *) this;
  next = 0;
}

template <class T>
Handle<T>::~Handle() {
  if(previous) previous->next = next;
  if(next) next->previous = previous;
  if(state.handle_list == (Handle<Value> *) this) {
    PIP_ASSERT(!next);
    state.handle_list = previous;
  }
}

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

///// (STD) Standard Scheme functions implemented in C++

// TYPENOTE
inline std::ostream& print_table(std::ostream& os, Table* t) {
  PIP_ASSERT(t->get_type() == TABLE);
  os << "#<table " << std::endl;
  for(size_t i = 0; i != t->chains->length; i++) {
    Value* chain = t->chains->data[i];
    if(chain)
      std::cout << " chain " << i << ":" << std::endl; 
    while(chain->get_type() == PAIR) {
      std::cout << "  " << chain->caar() << " => " << chain->cdar()
                << std::endl;
      chain = chain->cdr();
    }
  }
  return os << '>';
}

inline std::ostream& operator<<(std::ostream& os, Type& t) {
  switch(t) {
    case UPVALUE: os << "an upvalue"; break;
    case TRANSIENT: os << "bad type"; break;
    case PAIR: os << "a pair"; break;
    case VECTOR: os << "a vector"; break;
    case VECTOR_STORAGE: os << "a vector-storage"; break;
    case BLOB: os << "a blob"; break;
    case STRING: os << "a string"; break;
    case SYMBOL: os << "a symbol"; break;
    case EXCEPTION: os << "an exception"; break;
    case BOX: os << "a box"; break;
    case PROTOTYPE: os << "a scheme function"; break;
    case CLOSURE: os << "a scheme closure"; break;
    case NATIVE_FUNCTION: os << "a native function"; break;
    case TABLE: os << "a table"; break;
    case FIXNUM: os << "a fixnum"; break;
    case CONSTANT: os << "a constant"; break;
  }
  return os;
}

#define PIP_CHECK_TYPE(expect, number) \
  if(args[(number)]->get_type() != (expect)) { \
    return state.type_error(fn_name, (number), args[(number)], \
                            (expect), args[(number)]->get_type()); \
  }

inline Value* car(State& state, unsigned argc, Value* args[]) {
  static const char* fn_name = "car";
  PIP_CHECK_TYPE(PAIR, 0);
  return args[0]->car();
}

inline Value* cdr(State& state, unsigned argc, Value* args[]) {
  static const char* fn_name = "car";
  PIP_CHECK_TYPE(PAIR, 0);
  return args[0]->cdr();
}

inline void State::initialize_builtin_functions() {
  defun("car", &car, 1, false);
  defun("cdr", &cdr, 1, false);
}

// Output

inline std::ostream& operator<<(std::ostream& os, Value* x) {
  switch(x->get_type()) {
    case CONSTANT:
      if(x == PIP_TRUE) return os << "#t";
      if(x == PIP_FALSE) return os << "#f";
      if(x == PIP_EOF) return os << "#<eof>";
      if(x == PIP_NULL) return os << "()";
      if(x == PIP_UNSPECIFIED) return os << "#<unspecified>";
      break;
    case SYMBOL:
      return os << x->symbol_name()->string_data();
    case FIXNUM:
      return os << x->fixnum_value();
    case BLOB:
        return os << "#<blob " << x->blob_length() << '>';
    case STRING:
        return os << "\"" <<  x->string_data() << "\"";
    case VECTOR_STORAGE: return os << "#<vector-storage>" << std::endl;
    case VECTOR:
        os << "#(";
        for(size_t i = 0; i != x->vector_length(); i++) {
          os << x->vector_ref(i);
          if(i != x->vector_length() - 1) os << ' ';
        }
        return os << ')';
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
      return os << "#<exception " << x->exception_tag() << ' ' 
                << x->exception_message() << '>';
    }
    case PROTOTYPE: return os << "#<prototype>";
    case UPVALUE: return os << "#<upvalue "
                            << (x->upvalue_closed() ? "(closed) " : "")
                            << " " << x->upvalue() << ')' << std::endl;
    case TABLE: {
      Table* t = static_cast<Table*>(x);
      return os << "#<table entries: " << t->entries
                << " max-entries: " << t->max_entries << ">";
    }
    default:
      return os << "#<unknown value " << (size_t) x << " type: "
                << x->get_type() << ">";
  }
  return os;
}

} // pip

using pip::operator<<;

#endif
