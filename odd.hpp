// odd.hpp - scheme in (almost) one header

// Copyright (c) 2011-2012 ioddly
// Website: <http://ioddly.com/projects/odd/>
// Released under the Boost Software License:
// <http://www.boost.org/LICENSE_1_0.txt>

// Welcome to Odd Scheme!

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

#ifndef ODD_HPP
#define ODD_HPP

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <algorithm>
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
// ODD_DEBUG is explicitly defined.
#ifdef ODD_DEBUG
# define ODD_ASSERT(x) (assert(x))
#else
# define ODD_ASSERT(x) ((void)0)
#endif

// Try to give a graceful error message when internal errors occur
#define ODD_FAIL(desc) { std::cerr << desc << std::endl; ODD_ASSERT(!"failure"); }

// Checking for exceptions
#define ODD_CHECK(x) if((x)->active_exception()) return (x);

// Trace messages
#ifdef ODD_TRACE
# define ODD_GC_MSG(x) if(trace) std::cout << "GC: " << x << std::endl
# define ODD_CC_MSG(x) \
    if(state.trace) { \
      for(size_t _x = (depth); _x; _x--) \
        std::cout << '>'; std::cout << "CC: " << x << std::endl; \
    }
# define ODD_CC_VMSG(x) if(state.trace) ODD_CC_MSG(x)
# define ODD_CC_EMIT(x) if(state.trace) ODD_CC_MSG((last_insn) << ": " << x)

# define ODD_VM_MSG(x) \
  if(trace) { \
    for(size_t _x = (vm_depth); _x != 1; _x--) \
      std::cout << '>'; \
    std::cout << "VM: " << x << std::endl; \
  }

# define ODD_VM_VMSG(y) ODD_VM_MSG("(stack: " << f.si << ") " << y)
// Execute a particular instruction, includes instruction offset
# define ODD_VM_MSG_INSN(y) ODD_VM_MSG("INSN: " << ip-1 << " (stack: " << f.si << ") " << y)
#else
# define ODD_GC_MSG(x)
# define ODD_CC_MSG(x)
# define ODD_CC_VMSG(x) 
# define ODD_CC_EMIT(x)
# define ODD_VM_MSG(x) 
# define ODD_VM_VMSG(x)
#endif

// Heap alignment is both the default size of the heap and the threshold for
// allocating large objects separate from the normal heap
#ifndef ODD_HEAP_ALIGNMENT
# define ODD_HEAP_ALIGNMENT 4096 // 4KB
#endif

// The "load factor" is a percentage used to determine when to grow the heap.
// When the live objects in memory after a collection exceed this percentage,
// the heap is grown.
#ifndef ODD_LOAD_FACTOR
# define ODD_LOAD_FACTOR 77
#endif

namespace odd {

struct State;
struct Value;
struct String;
struct Symbol;
struct VectorStorage;
struct Table;

std::ostream& operator<<(std::ostream& os, odd::Value* x);
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
  if(f.s >= 1024) { f.s /= 1024; ending = 'G'; }
  return os << f.s << ending;
}

// Warning: do not change structure without changing how compiler encodes debuginfo
struct SourceInfo {
  SourceInfo(): file(0), line(0) {}
  unsigned file, line;
};

typedef std::pair<unsigned, SourceInfo> debug_t;

///// (OBJ) Object representation 

// <--
// The first thing a language implementation needs to do is determine how to
// represent the language's values in memory. It's hard to interact with
// objects when you don't even know what they look like! 

// So how do we represent a Scheme object (dynamically typed) with a C++
// structure? The easiest way would be to take advantage of C++'s polymorphism.
// But I've decided to go in a different direction, for performance reasons.
// Odd values can be either immediate (they are contained directly within an
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
#define ODD_CAST(klass, x) (ODD_ASSERT((x)->get_type() == odd::klass ::CLASS_TYPE), (klass *) (x))

// Constant values 
#define ODD_FALSE ((odd::Value*) 0)
#define ODD_TRUE ((odd::Value*) 2)
#define ODD_NULL ((odd::Value*) 6)
#define ODD_EOF ((odd::Value*) 10)
#define ODD_UNSPECIFIED ((odd::Value*) 18)

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
  // Other
  TABLE,
  SYNCLO,
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
    ODD_ASSERT(pointerp()); ODD_ASSERT(get_type() == type);
    return header & bit;
  }

  void set_header_bit(Type type, int bit) {
    ODD_ASSERT(get_type() == type);
    header += bit;
  }

  void unset_header_bit(Type type, int bit) {
    ODD_ASSERT(get_type() == type);
    header -= bit;
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

  static Value* to_boolean(bool i) { return i ? ODD_TRUE : ODD_FALSE; }
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
  Value* caadr() const;
  Value* cdadr() const;
  Value* caddr() const;

  Value* list_ref(size_t i) const;

  void set_car(Value*);
  void set_cdr(Value*);

  // Vectors
  Value** vector_data();
  Value* vector_ref(size_t i);
  void vector_set(size_t, Value*);
  size_t vector_length() const;
  VectorStorage* vector_storage() const;
  bool vector_memq(Value* x);

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

  const char* prototype_name() const;

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

  // Syntactic closures
  Value* synclo_expr() const;
  Table* synclo_env() const;
  Value* synclo_escaped_variables() const;
};

struct Pair : Value {
  Value *car, *cdr;
  // Source code information; only available when pair_has_source() is true
  SourceInfo src;
  
  static const Type CLASS_TYPE = PAIR;
};

Value* Value::car() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->car;
}

Value* Value::cdr() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr;
}

Value* Value::caar() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->car->car();
}

Value* Value::cadr() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr->car();
}

Value* Value::cddr() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr->cdr();
}

Value* Value::cdar() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->car->cdr();
}

Value* Value::cdadr() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr->car()->cdr();
}

Value* Value::caadr() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr->car()->car();
}

Value* Value::caddr() const {
  ODD_ASSERT(get_type() == PAIR);
  return static_cast<const Pair*>(this)->cdr->cdr()->car();
}

Value* Value::list_ref(size_t i) const {
  ODD_ASSERT(get_type() == PAIR);
  const Value* lst = this;
  while(i--) {
    lst = lst->cdr();
  }
  return lst ? lst->car() : ODD_FALSE;
}

void Value::set_car(Value* car_) { 
  ODD_ASSERT(get_type() == PAIR);
  static_cast<Pair*>(this)->car = car_;
}

void Value::set_cdr(Value* cdr_) {
  ODD_ASSERT(get_type() == PAIR);
  static_cast<Pair*>(this)->cdr = cdr_;
}

struct VectorStorage : Value {
  unsigned length;
  Value* data[1];

  static const Type CLASS_TYPE = VECTOR_STORAGE;
};

Value** Value::vector_storage_data() const {
  ODD_ASSERT(get_type() == VECTOR_STORAGE);
  return (Value**)static_cast<const VectorStorage*>(this)->data;
}

struct Vector : Value {
  VectorStorage* storage;
  unsigned capacity;

  static const Type CLASS_TYPE = VECTOR;
};

VectorStorage* Value::vector_storage() const {
  ODD_ASSERT(get_type() == VECTOR);
  return static_cast<const Vector*>(this)->storage;
}
Value** Value::vector_data() { return vector_storage()->data; }
Value* Value::vector_ref(size_t i) { return vector_storage()->data[i]; }
void Value::vector_set(size_t i, Value* v) { vector_storage()->data[i] = v; }
size_t Value::vector_length() const { return vector_storage()->length; }
bool Value::vector_memq(Value* x) {
  if(get_type() != VECTOR) return false;
  for(size_t i = 0; i != vector_length(); i++) {
    if(vector_ref(i) == x) return true;
  }
  return false;
}

struct Blob : Value {
  unsigned length;
  char data[1];

  static const Type CLASS_TYPE = BLOB;
};

const char* Value::blob_data() const {
  ODD_ASSERT(get_type() == BLOB); return static_cast<const Blob*>(this)->data;
}

unsigned Value::blob_length() const {
  ODD_ASSERT(get_type() == BLOB);
  return static_cast<const Blob*>(this)->length;
}

template <class T>
unsigned Value::blob_length() const {
  ODD_ASSERT(get_type() == BLOB); 
  return static_cast<const Blob*>(this)->length / sizeof(T); 
}

template <class T> 
T Value::blob_ref(unsigned i) const { 
  ODD_ASSERT(get_type() == BLOB); return ((T*) 
      static_cast<const Blob*>(this)->data)[i]; 
}

template <class T> 
void Value::blob_set(unsigned i, T& val) const {
  ODD_ASSERT(get_type() == BLOB);
  ((T*) static_cast<const Blob*>(this)->data)[i] = val;
}

struct String : Blob {
  static const Type CLASS_TYPE = STRING;
};

const char* Value::string_data() const {
  ODD_ASSERT(get_type() == STRING); return static_cast<const String*>(this)->data;
}
unsigned Value::string_length() const {
  ODD_ASSERT(get_type() == STRING);
  return static_cast<const String*>(this)->length;
}

struct Symbol : Value {
  Value *name, *value;

  static const Type CLASS_TYPE = SYMBOL;
};

Value* Value::symbol_name() const {
  ODD_ASSERT(get_type() == SYMBOL);
  return static_cast<const Symbol*>(this)->name;
}

Value* Value::symbol_value() const {
  ODD_ASSERT(get_type() == SYMBOL);
  return static_cast<const Symbol*>(this)->value;
}

struct Exception : Value {
  Symbol* tag;
  String* message;
  Value* irritants;
  
  static const Type CLASS_TYPE = EXCEPTION;
};

Symbol* Value::exception_tag() const {
  ODD_ASSERT(get_type() == EXCEPTION);
  return static_cast<const Exception*>(this)->tag; 
};

String* Value::exception_message() const {
  ODD_ASSERT(get_type() == EXCEPTION);
  return static_cast<const Exception*>(this)->message;
};

Value* Value::exception_irritants() const {
  ODD_ASSERT(get_type() == EXCEPTION);
  return static_cast<const Exception*>(this)->irritants; 
};

struct Box : Value {
  Value* value;
  SourceInfo src;

  static const Type CLASS_TYPE = BOX;
};

Value* Value::box_value() const {
  ODD_ASSERT(get_type() == BOX);
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

const char* Value::prototype_name() const {
  ODD_ASSERT(get_type() == PROTOTYPE);
  return static_cast<const Prototype*>(this)->name
    ? static_cast<const Prototype*>(this)->name->string_data() : "anonymous";
}

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
  Upvalue* u = ODD_CAST(Upvalue, this);
  return upvalue_closed() ? u->converted : *(u->local);
}

void Value::upvalue_set(Value* v) {
  Upvalue* u = ODD_CAST(Upvalue, this);
  if(upvalue_closed())
    u->converted = v;
  else
    (*u->local) = v;
}

void Value::upvalue_close() {
  Upvalue* u = ODD_CAST(Upvalue, this);
  set_header_bit(UPVALUE, UPVALUE_CLOSED_BIT);
  u->converted = *u->local;
}

struct NativeFunction : Value {
  typedef Value* (*ptr_t)(State&, unsigned, Value* args[], VectorStorage*);

  VectorStorage* closure;
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

// Syntactic closures
struct Synclo : Value {
  Table* env;
  Vector* escaped_variables;
  Value* expr;

  static const Type CLASS_TYPE = SYNCLO;
};

Value* Value::synclo_expr() const {
  ODD_ASSERT(get_type() == SYNCLO);
  return static_cast<const Synclo*>(this)->expr;
}

Table* Value::synclo_env() const {
  ODD_ASSERT(get_type() == SYNCLO);
  return static_cast<const Synclo*>(this)->env;
}

Value* Value::synclo_escaped_variables() const {
  ODD_ASSERT(get_type() == SYNCLO);
  return static_cast<const Synclo*>(this)->escaped_variables;
}

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

  void swap(Value*& other) {
    Value* s = ref;
    ref = (T*) other;
    other = s;
  }
};

// This structure is used in the GC tracking macros to take a normal
// pointer to a Odd value of any type and obtain a Value** pointer to it
struct FrameHack {
  // TYPENOTE
#define ODD_(t) FrameHack(t *& ref): ref((Value**) &ref) {}
  ODD_(Value)
  ODD_(Pair)
  ODD_(Blob)
  ODD_(Symbol)
  ODD_(Vector)
  ODD_(VectorStorage)
  ODD_(String)
  ODD_(Exception)
  ODD_(Box)
  ODD_(Prototype)
  ODD_(Closure)
  ODD_(Upvalue)
  ODD_(NativeFunction)
  ODD_(Synclo)
  ODD_(Table)
#undef ODD_
  
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
#define ODD_E_FRAME(state, ...) \
  odd::FrameHack __odd_frame_hacks[] = { __VA_ARGS__ }; \
  odd::Frame __odd_frame((state), (FrameHack*) __odd_frame_hacks, \
      sizeof(__odd_frame_hacks) / sizeof(odd::FrameHack))

// For general use

// Assumes there is a variable State& state
#define ODD_FRAME(...) ODD_E_FRAME((state), __VA_ARGS__)
#define ODD_S_FRAME(...) ODD_E_FRAME((*this), __VA_ARGS__)

// Some forward declarations for state
#define ODD_FUNCTION(name) \
  inline Value* name (State& state, unsigned argc, Value* args[], \
                      VectorStorage* closure)

ODD_FUNCTION(apply_macro);

struct State {
struct VMFrame;

///// (GC) Garbage collector

// <--
// Odd's garbage collector is a mark-and-don't-sweep collector. The allocator
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
// Odd's case, the mark field is stored alongside type and other flags in a
// space efficient manner). The main downside is fragmentation.

// How does Odd mark live memory? It starts with the program's "roots" and
// marks all their children and so on. But how do we find the roots? They are
// explicitly registered with the garbage collector using two methods: Frames
// and Handles. A Frame is a stack-allocated structure that keeps track of as
// many variables as needed. You create a frame in each function you want to
// track variables in like so: ODD_FRAME(var1, var2). A Handle is a
// heap-allocated structure that keeps track of one variable, and is useful
// when your variables have lifetimes beyond the execution of a single
// function.

// How does Odd get memory from the operating system? It starts by using a
// small amount of memory (4 kilobytes). If, after a collection, more than a
// certain amount of memory is in use (the "load factor", by default 77%), it
// allocates twice as much memory to avoid collecting too often. It can also
// allocate more memory if a very large object (over 4kb) is allocated.

// In addition to normal collection, Odd has support for doing a full
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

// Actual heap
std::vector<Block*> blocks;
// Garbage collector root set
Handle<Value>* handle_list;
std::vector<VMFrame*> vm_frames;
Frame* frame_list;
// Print a bunch of messages about compiler, virtual machine, etc, only works with ODD_DEBUG
bool trace;
// Does exactly what it says. Helpful for flushing out garbage collector bugs. Expensive.
bool collect_before_every_allocation;
// If false, will not optimize tail calls. Handy for debugging purposes.
bool optimize_tail_calls;
// Garbage collector data
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
      case STRING: case BLOB:
        return;
      // One pointer
      case NATIVE_FUNCTION: case TABLE: case VECTOR: case BOX:
        x = static_cast<Box*>(x)->value;
        continue;
      // Two pointers 
      case CLOSURE: case PAIR: case SYMBOL:
        recursive_mark(static_cast<Pair*>(x)->cdr);
        x = static_cast<Pair*>(x)->car;
        continue;
      // Three pointers
      case SYNCLO:
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
        ODD_FAIL("recursive_mark got bad type " << x->get_type());
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

    ODD_ASSERT(sweep_cursor >= block->begin && sweep_cursor <= block->end);
    while(sweep_cursor != block->end) {
      sweep_cursor_v = (Value*) sweep_cursor;
      // If this space is used, skip it
      if(markedp(sweep_cursor_v)) {
        sweep_cursor += sweep_cursor_v->get_size_unsafe();
        sweep_cursor_v = (Value*) sweep_cursor;
        // Should never go past the end of the heap
        ODD_ASSERT(sweep_cursor <= block->end);
        continue;
      }

      // Free space, figure out how large it is
      size_t hole_size = sweep_cursor_v->get_size_unsafe();
      // Should never have a hole that can't fit object prefix
      ODD_ASSERT(hole_size >= sizeof(Value));
    
      // If the hole is not big enough, see if we can coalesce holes
      // in front of it
      while(hole_size < required) {
        Value* next = (Value*) (sweep_cursor + hole_size);

        if(next == (Value*) block->end) {
          break;
        }
        ODD_ASSERT((char*)next <= block->end);

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
          ODD_ASSERT(!markedp(sweep_cursor_v));
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

        ODD_ASSERT(address_v->get_size_unsafe() == actual_size);
        ODD_ASSERT(markedp(address_v));

        // Done!
        break;
      }

      // The hole is not big enough, mark as used and move on
      sweep_cursor_v->flip_mark_unsafe();
      ODD_ASSERT(markedp(sweep_cursor_v));
    
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
    ODD_ASSERT(markedp(address_v));
  return address;
}

// If collection is called early, finish iterating over and marking the heap
void finish_mark() {
  while(block_cursor != blocks.size()) {
    Block* b = blocks[block_cursor];
    ODD_ASSERT(sweep_cursor >= b->begin && sweep_cursor <= b->end);
    while(sweep_cursor != b->end) {
      Value* sweep_cursor_v = (Value*) sweep_cursor;
      if(!markedp(sweep_cursor_v)) {
        sweep_cursor_v->flip_mark_unsafe();
        ODD_ASSERT(markedp(sweep_cursor_v));
      }
      sweep_cursor += sweep_cursor_v->get_size_unsafe();
      ODD_ASSERT(sweep_cursor <= b->end);
    }
    block_cursor++;
    if(block_cursor != blocks.size())
      sweep_cursor = blocks[block_cursor]->begin;
  }
}

// Mark all live memory
void mark_heap() {
  for(Frame* f = frame_list; f != NULL; f = f->previous)
    for(size_t i = 0; i != f->root_count; i++)
      recursive_mark(*f->roots[i]);

  for(Handle<Value>* i = handle_list; i != NULL; i = i->previous)
    recursive_mark(i->ref);

  for(size_t i = 0; i != safestack.size(); i++)
    recursive_mark(safestack[i]);
  
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
        // If this symbol has its value set, we'll consider it a root
        if(ODD_CAST(Symbol, cell->cdar())->value != ODD_FALSE) {
          recursive_mark(cell->cdar());
          continue;
        }        
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
    if(symbol_table->chains->data[i] == ODD_NULL)
      symbol_table->chains->data[i] = ODD_FALSE;
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
  if(pressure >= ODD_LOAD_FACTOR || force_request) {
    // We're gonna grow
    size_t new_block_size = (heap_size);
  
    ODD_ASSERT(new_block_size >= request);

    // Make absolutely sure we can accomodate that request
    if(new_block_size < request)
      new_block_size = align(ODD_HEAP_ALIGNMENT, (heap_size) + request);

    Block* b = new Block(new_block_size, true, mark_bit);
    blocks.push_back(b);

    //ODD_ASSERT(!markedp((Value*)b->begin));

    heap_size += new_block_size;

    ODD_GC_MSG("growing heap from " <<
        FriendlySize(heap_size - new_block_size)
        << " to " << FriendlySize(heap_size)
        << " because of " <<
        (pressure >= ODD_LOAD_FACTOR ? "pressure" : "allocation failure"));
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

  if(size >= ODD_HEAP_ALIGNMENT) {
    size = align(ODD_HEAP_ALIGNMENT, size);
    Block* block = new Block(size, false, mark_bit);
    blocks.push_back(block);
    Value* x = (Value*) block->begin;
    x->set_type_unsafe(type);
    x->flip_mark_unsafe();
    ODD_ASSERT(markedp(x));

    heap_size += size;

    ODD_GC_MSG("growing heap from " << FriendlySize(heap_size - size)
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
        ODD_ASSERT(!"out of memory");
      }
    }
  }

  Value* x = (Value*) address;

  x->set_type_unsafe(type);

  ODD_ASSERT(x->get_size_unsafe() >= size);
  ODD_ASSERT(markedp(x));
  ODD_ASSERT(x->get_type_unsafe() == type);
  ODD_ASSERT(x->pointerp());

  return x;
}

// The actual entry point of the garbage collector
template <class T> T* allocate(ptrdiff_t additional = 0) {
  return static_cast<T*>(allocate(static_cast<Type>(T::CLASS_TYPE), sizeof(T) + additional));
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
    case SYNCLO: size = sizeof(Synclo); break;
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
      ODD_FAIL("minimum_size type should not occur: " << x->get_type());
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
  Block* heap = new Block(align(ODD_HEAP_ALIGNMENT, heap_size),
                          false, mark_bit);

  // Loop over live memory, allocating from the new block. The new allocation
  // is called the "forwarded object" and the object's 'size" field is
  // re-used to store a pointer to this forwarded object. (The newly
  // allocated object contains the size information, for when it's needed)

  char* bump = heap->begin;

  ODD_GC_MSG("copying heap");
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

  ODD_GC_MSG("updating roots with new pointers");

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

  ODD_GC_MSG("updating heap with new pointers");
  char* sweep = heap->begin;
  while(sweep != bump) {
    Value* x = (Value*) sweep;
    sweep += x->size;
#define ODD_FWD(type, field) update_forward((Value**) &(((type*) x)->field))
    switch(x->get_type_unsafe()) {
      // TYPENOTE
      // Atomic
      case STRING: case BLOB:
        continue;
      // One pointer
      case NATIVE_FUNCTION: case VECTOR: case BOX: case TABLE:
        ODD_FWD(Box, value);
        continue;
      // Two pointers
      case CLOSURE: case PAIR: case SYMBOL:
        update_forward(&((Pair*) x)->car);
        update_forward(&((Pair*) x)->cdr);
        continue;
      // Three pointers
      case SYNCLO:
      case EXCEPTION:
        ODD_FWD(Exception, tag);
        ODD_FWD(Exception, message);
        ODD_FWD(Exception, irritants);
        continue;
      // Five pointers
      case PROTOTYPE:
        ODD_FWD(Prototype, code);
        ODD_FWD(Prototype, debuginfo);
        ODD_FWD(Prototype, local_free_variables);
        ODD_FWD(Prototype, upvalues);
        ODD_FWD(Prototype, name);
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
        ODD_FAIL("compact encountered bad type: " << x->get_type()); 
#undef ODD_FWD
    }
  }

  ODD_GC_MSG("forwarded all program roots");
  
  // Delete heap and replace with new heap
  delete_blocks();
  blocks.push_back(heap);
  reset_sweep_cursor();

  // Note the size of the hole at the end of the heap
  // Ensure we have enough room at the end for a Value
  ODD_ASSERT((size_t)(heap->end - bump) >= align(POINTER_ALIGNMENT,
        sizeof(Value*)));
  // Determine the size
  Value* hole = (Value*) bump;
  hole->header = TRANSIENT;
  hole->size = (heap->end - bump);
}

///// (RT) Runtime

// <--
// Each instance of the Odd runtime is contained within the rather monolithic
// State structure.  Odd is completely re-entrant. 
// -->

State(): 
  // Garbage collector
  handle_list(0), frame_list(0), 
  trace(false), collect_before_every_allocation(false), optimize_tail_calls(true), mark_bit(1),
  heap_size(ODD_HEAP_ALIGNMENT),
  block_cursor(0),  live_at_last_collection(0), collections(0),
  sweep_cursor(0),

  // Reader
  source_counter(1),
 
  // Compiler

  // An environment containing special forms, core functions, and all the
  // other building blocks necessary for Odd
  core_module(*this),
  // User interaction environment (eg REPL, one-off evaluations)
  user_module(*this),
  
  // Virtual machine
  vm_depth(0),
  vm_trampoline_function(0),

  modules(*this) {

  // Initialize garbage collector
  Block* first = new Block(ODD_HEAP_ALIGNMENT * 2, false, mark_bit);
  blocks.push_back(first);
  sweep_cursor = first->begin;

  // Initialize globals
  static const char* global_symbols[] = {
    // Compiler symbols
    "special",
    "variable",
    "upvalue",
    // Hidden module variables
    "#parent",
    "#unqualified-imports",
    "#qualified-imports",
    "#module-name",
    "#global-environment",
    "#exports",
    "#exporting",
    "#export-parent",
    // Exception tags
    "odd-read",
    "odd-compile",
    "odd-eval",
    "odd-table",
    "odd-syntax",
    "odd-type",
    // Special forms
    "def",
    "access",
    "set",
    "brace",
    "lambda",
    "#named-lambda",
    "if",
    "defsyntax",
    "module",
    "quote",
    "import",
    "public",
    "private",
    "export",
    // Other
    "quasiquote",
    "unquote",
    "unquote-splicing",
    "*"
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
  core_module = make_env(ODD_FALSE, "#odd#core");

  for(size_t i = S_DEF; i != S_EXPORT+1; i++) {
    env_define(*core_module, global_symbol(static_cast<Global>(i)),
               global_symbol(S_SPECIAL), true);
  }

  user_module = make_env(*core_module, "#user");

  // Create module table
  modules = make_table();

  initialize_builtin_functions();

  register_module("#odd#core", *core_module);
  register_module("#user", *user_module);

  toplevel_cc = new Compiler(*this, 0);
  toplevel_cc->enter_module(table_get(*user_module, global_symbol(S_MODULE_NAME)));
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
  // Compiler forms
  S_SPECIAL,
  S_VARIABLE,
  S_UPVALUE,
  // For environments
  S_PARENT,
  S_UNQUALIFIED_IMPORTS,
  S_QUALIFIED_IMPORTS,
  S_MODULE_NAME,
  S_GLOBAL_ENVIRONMENT,
  S_EXPORTS,
  S_EXPORTING,
  S_EXPORT_PARENT,
  // Exceptions
  S_ODD_READ,
  S_ODD_COMPILE,
  S_ODD_EVAL,
  S_ODD_TABLE,
  S_ODD_SYNTAX,
  S_ODD_TYPE,
  // Special forms
  S_DEF,
  S_ACCESS,
  S_SET,
  S_BRACE,
  S_LAMBDA,
  S_NAMED_LAMBDA,
  S_IF,
  S_DEFSYNTAX,
  S_MODULE,
  S_QUOTE,
  S_IMPORT,
  S_PUBLIC,
  S_PRIVATE,
  S_EXPORT,
  // End special forms
  S_QUASIQUOTE,
  S_UNQUOTE,
  S_UNQUOTE_SPLICING,
  S_ASTERISK,
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
  ODD_S_FRAME(value, box);
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
  ODD_S_FRAME(p, car, cdr);
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

Pair* list_copy(Value* lst) {
  Value *lst2head = 0, *lst2tail = 0;
  ODD_S_FRAME(lst, lst2head, lst2tail);
  while(lst->get_type() == PAIR) {
    append_m(&lst2head, &lst2tail, lst->car());
    lst = lst->cdr();
  }
  return static_cast<Pair*>(lst2head);
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
    sym = ODD_CAST(Symbol, search);
  } else {
    // Symbol doesn't exist yet, intern it
    String* cpy = 0;
    ODD_S_FRAME(sym, string, cpy);
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
  ODD_S_FRAME(v, s);
  v = allocate<Vector>();
  v->capacity = capacity;
  s = allocate<VectorStorage>(array_size<Value*>(capacity));
  v->storage = s;
  return v;
}

Exception* make_exception(Symbol* tag, String* message, Value* irritants) {
  Exception* e = 0;
  ODD_S_FRAME(e,tag,message,irritants);
  e = allocate<Exception>();
  e->tag = tag;
  e->message = message;
  e->irritants = irritants;
  e->set_header_bit(EXCEPTION, Value::EXCEPTION_ACTIVE_BIT);
  return e;
}

Exception* make_exception(Global g, const std::string& msg) {
  String* smsg = make_string(msg);
  return make_exception(global_symbol(g), smsg, ODD_FALSE);
}

void table_setup(Table* table, unsigned size_log2) {
  VectorStorage* chains = 0;
  ODD_S_FRAME(table, chains);
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
  ODD_S_FRAME(table, chains);

  table = allocate<Table>();
  table_setup(table, size_log2);
  
  return table;
}

NativeFunction* make_native_function(NativeFunction::ptr_t ptr,
                                     unsigned arguments,
                                     bool variable_arity,
                                     VectorStorage* closure = 0) {
  NativeFunction* fn = 0;
  ODD_S_FRAME(fn, closure);
  fn = allocate<NativeFunction>();
  fn->pointer = ptr;
  fn->arguments = arguments;
  fn->closure = closure;
  if(variable_arity)
    fn->set_header_bit(NATIVE_FUNCTION, Value::NATIVE_FUNCTION_VARIABLE_ARITY_BIT);
  return fn;
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
      return strcmp(ODD_CAST(String, a)->data, ODD_CAST(String, b)->data)==0;
    default: break;
  }
  std::cerr << "equals not implemented for type " << a->get_type();
  ODD_ASSERT(!"equals not implemented for type");
  return false;
}

void defun(const std::string& cname, NativeFunction::ptr_t ptr,
           unsigned arguments, bool variable_arity) {
  NativeFunction* fn = 0;
  Symbol* name = 0;
  Symbol* qualified_name= 0;
  ODD_S_FRAME(fn, name, qualified_name);
  fn = make_native_function(ptr, arguments, variable_arity);
  name = make_symbol(cname);
  env_define(*core_module, name, name, true);
  qualified_name = ODD_CAST(Symbol, env_qualify_name(*core_module, name));
  qualified_name->value = fn;
}

VectorStorage* vector_storage_realloc(size_t capacity, VectorStorage* old) {
  VectorStorage* data = 0;
  ODD_S_FRAME(old, data);

  data = allocate<VectorStorage>(array_size<Value*>(capacity));
  if(old) {
    data->length = old->length;
    memcpy(data->data, old->data, old->length * sizeof(Value*));
  }
  return data;
}

// Combine two vectors (also accepts #f as argument and ignores it)
Vector* vector_combine(Vector* v1, Vector* v2) {
  if(!v1 && !v2) return (Vector*) ODD_FALSE;
  Vector* n = 0;
  ODD_S_FRAME(n, v1, v2);
  // Pre-calculate size of new vector
  size_t capacity = (v1->get_type() == VECTOR ? v1->vector_length() : 0) + \
                    (v2->get_type() == VECTOR ? v2->vector_length() : 0);
  n = make_vector(capacity);
  if(v1->get_type() == VECTOR)
    for(size_t i = 0; i != v1->vector_length(); i++)
      vector_append(n, v1->vector_ref(i));
  if(v2->get_type() == VECTOR)
    for(size_t i = 0; i != v2->vector_length(); i++)
      vector_append(n, v2->vector_ref(i));
  return n;
}

unsigned vector_append(Vector* vec, Value* value) {
  vec->storage->data[vec->storage->length++] = value;
  if(vec->storage->length == vec->capacity) {
    VectorStorage* s = 0;
    ODD_S_FRAME(vec, s);
    s = vector_storage_realloc(vec->capacity * 2, vec->storage);
    vec->capacity *= 2;
    vec->storage = s;
  }
  return vec->storage->length - 1;
}

static void unwrap_source_info(Value* exp, SourceInfo& info) {
  if(exp->get_type() == PAIR && exp->pair_has_source()) {
    info = ODD_CAST(Pair, exp)->src;
  } else if(exp->get_type() == BOX && exp->box_has_source()) {
    info = ODD_CAST(Box, exp)->src;
  }
}

void format_source_error(Value* exp, const std::string& msg,
                        std::string& out) {
  SourceInfo info;
  unwrap_source_info(exp, info);
  if(info.file) {
    std::ostringstream ss;
    ss << source_names[info.file] << ':' << info.line << ": " << msg;
    // TODO: Print source code? 
    // "       " << source_contents[info.file]->at(info.line-1);
    out = ss.str();
  } else {
    out = msg;
  }
}

// Dealing with symbols which are boxed and contain source code location
// info    
static Value* unbox(Value* x) {
  if(x->get_type() == BOX) return x->box_value();
  return x;
}

// Lists

// Returns true if something is a true list (either () or a list with no dots in it)
static bool listp(Value* lst) {
  if(lst == ODD_NULL) return true;
  if(lst->get_type() != PAIR) return false;
  while(lst->get_type() == PAIR) {
    lst = lst->cdr();
    if(lst == ODD_NULL) return true;
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
  ODD_S_FRAME(h, t, swap, value);
  swap = cons(value, ODD_NULL);
  if(!h) {
    (*head) = swap;
    (*tail) = swap;
  } else {
    t->set_cdr(swap);
    (*tail) = swap;
  }
}

// Hash tables

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
  return make_exception(S_ODD_TABLE, ss.str());
}

static ptrdiff_t hash_index(Table* table, Value* key, bool& unhashable) {
  ptrdiff_t hash = hash_value(key, unhashable);
  return hash & (table->chains->length - 1);
}

void table_grow(Table* table) {
  // Create new storage
  VectorStorage* old_chains = table->chains;
  Value *chain = 0;
  ODD_S_FRAME(table, old_chains, chain);
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
  ODD_ASSERT(table->get_type() == TABLE);    

  Value* chain = 0;
  ODD_S_FRAME(table, key, value, chain);

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
    chain = cons(chain, ODD_NULL);
  }

  // Insert chain
  table->chains->data[index] = chain;
  table->entries++;
  return ODD_FALSE;
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
  return ODD_FALSE;
}

Value* table_set(Table* table, Value* key, Value* value) {
  ODD_ASSERT(table->get_type() == TABLE);
  Value* cell = table_get_cell(table, key);
  if(cell && !cell->active_exception())
    cell->set_cdr(value);
  return cell;
}

Value* table_get(Table* table, Value* key, bool& found) {
  ODD_ASSERT(table->get_type() == TABLE);
  Value* cell = table_get_cell(table, key);
  if(cell) {
    found = true;
    return cell->cdr();
  } else {
    found = false;
    return ODD_FALSE;
  }
}

bool table_has_key(Table* table, Value* key) {
  bool found;
  (void) table_get(table, key, found);
  return found;
}

Value* table_get(Table* table, Value* key) {
  bool found;
  Value* ret = table_get(table, key, found);
  assert(found);
  return ret;
}

bool table_contains(Table* table, Value* key) {
  ODD_ASSERT(table->get_type() == TABLE);
  return table_get_cell(table, key) != ODD_FALSE;
}

///// (READ) Expression reader and source code tracking 

// <--
// Before reading expressions from a source (which may be a file or a string
// provided by e.g. a program that embeds Odd, or a user entering expressions
// at the command line), the source is first "registered" with the Odd
// runtime. It receives a unique numeric identifier and the entire text is
// read and saved. This is done in order to provide helpful error messages at
// every stage of program execution.

// The reader itself is fairly simple. It's split into a tokenizer and a
// parser; the tokenizer does most of the heavy lifting. The only parsing
// required is reading lists. The tokenizer is capable of previewing a single
// token.

// The reader attaches source code information to lists and symbols. In Odd,
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
    return c == '(' || c == ')' || c == '@' ||
      c == EOF || isspace(c) || c == '.' || c == '#' || c == '"'
      || quotep(c) || c == '{' || c == '}' || c == '[' || c == ']' || c == '.';
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
    TK_LOOKAHEAD = 0,
    TK_EOF = 1,
    TK_FIXNUM = 2,
    TK_WHITESPACE = 3,
    TK_NEWLINE = 4,
    TK_LPAREN = 5,
    TK_RPAREN = 6,
    TK_LBRACE = 7,
    TK_RBRACE = 8,
    TK_LBRACKET = 9,
    TK_RBRACKET = 10,
    TK_SYMBOL = 11,
    TK_STRING = 12,
    TK_TRUE = 13,
    TK_FALSE = 14,
    TK_EXCEPTION = 15,
    TK_DOT = 16,
    TK_QUOTE = 17,
    TK_QUASIQUOTE = 18,
    TK_UNQUOTE = 19,
    TK_UNQUOTE_SPLICING = 20
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
  Token lex_error(const std::string& msg, size_t sline = 0) {
    std::ostringstream ss;
    if(!sline) sline = line;
    ss << state.source_names[file] << ':' << sline << ": " << msg;
    token_value = state.make_exception(State::S_ODD_READ, ss.str());
    return TK_EXCEPTION;
  }

  Value* parse_error(const std::string& msg) {
    std::ostringstream ss;
    ss << state.source_names[file] << ':' << line << ": " << msg;
    return state.make_exception(State::S_ODD_READ, ss.str());
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

        // Comments (are handled here because '/' can be a symbol as well as the beginning of a comment)
        if(c == '/') {
          c = getc();
          if(c == '/') {
            while(1) { 
              c = getc();
              if(c == '\n' || c == EOF)
                break;
            }
            continue;
          } else if(c == '*') {
            // Multiline comments
            // TODO: Nesting
            while(1) {
              c = getc();
              if(c == '*') {
                c = getc();
                if(c == '/') break;
                else if(c == EOF) return lex_error("unexpected end of file in multiline comment");
              }
            }
            continue;
          } else {
            token_buffer += '/';
          }
        }

        token_buffer += c;

        while((c = getc())) {
          if(symbolp(c)) {
            token_buffer += c;
          } else {
            ungetc(c);
            
            Symbol* s = 0;
            ODD_FRAME(s);
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
        case '[': return TK_LBRACKET;
        case ']': return TK_RBRACKET;
        case '{': return TK_LBRACE;
        case '}': return TK_RBRACE;
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
          size_t sline = line;
          while((c = getc())) {
            if(c == EOF) {
              return lex_error("unexpected end of file in string", sline);
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
            case 't': return TK_TRUE;
            case 'f': return TK_FALSE;
            case EOF: return lex_error("unexpected EOF after #");
            case '\n': return lex_error("unexpected newline after #");
            default: {
              std::string msg = "did not expect \'";
              msg += c;
              msg += '\'';
              msg += " after #";
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
    token_value = ODD_FALSE;
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
#define ODD_READ_CHECK_TK(x) if((x) == TK_EXCEPTION) return pop_token_value(); 

enum ListType { 
  LITERAL,
  APPLICATION_ARGUMENTS,
  BRACE_CLAUSE
};

Value* read_list(ListType type, Value* initial = NULL, bool chain_applications = true) {
  Token end_delimiter = (type == BRACE_CLAUSE) ? TK_RBRACE : ((type == LITERAL) ? TK_RBRACKET : TK_RPAREN);
  // This function is in charge of reading lists of elements. It's
  // quite hairy because it handles function applications and list
  // literals, and tracks head and tail of the list so there is no
  // need to reverse it later.
  Value *head = NULL, *tail = NULL, *elt = NULL, *swap = NULL;

  // Save the line this list begins on for later
  size_t line = Reader::line;

  ODD_FRAME(initial, head, tail, elt, swap);

  // If we already have an element to start off with
  if(initial != NULL) {
    swap = state.cons(initial, ODD_NULL);
    head = tail = swap;
    swap = NULL;
  }

  // If this involves braces
  if(type == BRACE_CLAUSE) {
    // Create new list
    swap = state.cons(state.global_symbol(State::S_BRACE), ODD_NULL);
    // head may already exist, if this is a function application
    // with no normal arguments and only brace clauses
    if(head != NULL) {

      elt = state.cons(swap, ODD_NULL);
      head->set_cdr(elt);
      tail = swap;
    } else {
      head = tail = swap;
    }
  }

  Token token;
  // Read elements
  while(1) {
    token = peek_token(true, true);
    ODD_READ_CHECK_TK(token);

    // Exception

    // Should never encounter EOF in list
    if(token == TK_EOF) {
      std::ostringstream msg;
      msg << "unterminated ";
      msg << ((type == BRACE_CLAUSE) ? "brace clause" : ((type == APPLICATION_ARGUMENTS) ? "function call" : "list"));
      msg << " beginning on line " << line;
      return parse_error(msg.str().c_str());
    }

    // End of list
    if(token == end_delimiter) {
      discard_lookahead();
      break;
    }

    // Dotted list
    if(token == TK_DOT) {
      if(!head) {
        return parse_error("dot at beginning of list");
      } else {
        discard_lookahead();
        if(type == LITERAL) {
          break;
        } else {
          return parse_error("dot outside of list literal");
        }
      }
    }

    // Finally we can actually read a list element. Create a new
    // cell.
    elt = read();
    ODD_CHECK(elt);

    swap = (Value*) state.cons(elt, ODD_NULL);

    if(!head) {
      // If this is the first value in the list, it's both head
      // and tail
      head = tail = swap;
    } else {
      // Append this to the end of the list
      tail->set_cdr(swap);
      // And now it's the tail
      tail = swap;
    }
  }

  // Append brace clauses to function applications
  if(type == APPLICATION_ARGUMENTS) {
    while(1) {
      // Skip delimiters
      Token token2 = peek_token();
      ODD_READ_CHECK_TK(token2);
      
      // If there is a brace clause after this list
      if(token2 == TK_LBRACE) {
        // Drop brace
        discard_lookahead();
        // Read contents
        elt = read_list(BRACE_CLAUSE, NULL, false);

        ODD_CHECK(elt);

        // Create new list containing contents
        swap = state.cons(elt, ODD_NULL);
        // Append brace clause to function application
        tail->set_cdr(swap);
        tail = swap;
      } else {
        break;
      }
    }
  }

  // Allow application of return value
  if(type != LITERAL && chain_applications) {
    // We can't ignore whitespace because it might conflict with
    // list syntax
    Token token2 = peek_token(false, false);
    ODD_READ_CHECK_TK(token2);

    if(token2 == TK_LPAREN) {
      while(token2 == TK_LPAREN) {
        // Drop (
        discard_lookahead();
        // Read argument list
        elt = read_list(APPLICATION_ARGUMENTS, NULL, false);
        ODD_CHECK(elt);
        // Take this function application and apply its result
        swap = state.cons(head, elt);
        head = swap;
        token2 = peek_token();
        ODD_READ_CHECK_TK(token2);
      }
    }
  }

  // For list literals
  if(type == LITERAL) {
    if(token == TK_DOT) {
      token = peek_token();
      ODD_READ_CHECK_TK(token);

      if(token == TK_EOF) {
        return parse_error("unexpected end of file in dotted list");
      } else if(token == TK_RBRACKET) {
        return parse_error("dotted list ended early");
      } else {
        elt = read();
        ODD_CHECK(elt);
        tail->set_cdr(elt);
      }
      token = peek_token();
      ODD_READ_CHECK_TK(token);
      if(token != TK_RBRACKET) {
        return parse_error("more than one element at the end of a dotted list");
      }
      discard_lookahead();
    }
  }

  // Re-use old variables to attach source code information
  if(head->get_type() == PAIR) {
    swap = head->car();
    elt = head->cdr();
    
    head = state.cons(swap, elt, true);
    ODD_CAST(Pair, head)->src.file = file;
    ODD_CAST(Pair, head)->src.line = line;
  }

  return head == ODD_FALSE ? ODD_NULL : head;
}

Value* read_access(Value* sym) {
  Token tk;
  Value *lst_h = 0, *lst_t = 0;
  ODD_FRAME(sym, lst_h, lst_t);

  // Create list
  state.append_m(&lst_h, &lst_t, state.global_symbol(State::S_ACCESS));
  state.append_m(&lst_h, &lst_t, sym);

  while(true) {
    tk = peek_token(false, false);
    if(tk == TK_DOT) {
      discard_lookahead();
      tk = peek_token(false, false);
      if(tk == TK_SYMBOL) {
        sym = *token_value;
        state.append_m(&lst_h, &lst_t, sym);
        discard_lookahead();
      } else {
        return parse_error("dot after symbol must be followed immediately by another symbol");
      }
    } else {
      break;
    }
  }
  token_value = lst_h;
  return check_function_application(lst_h);
}

Value* check_function_application(Value* save_token) {
  Token token2 = peek_token();
  ODD_READ_CHECK_TK(token2);

  // There is a function application
  if(token2 == TK_LPAREN) {
    discard_lookahead();
    save_token = read_list(APPLICATION_ARGUMENTS, pop_token_value());
    ODD_CHECK(save_token);
    return save_token;
  }
  
  // Check for braces
  if(token2 == TK_LBRACE) {
    discard_lookahead();
    save_token = read_list(BRACE_CLAUSE, pop_token_value());
    ODD_CHECK(save_token);
    return save_token;
  }

  return save_token;
}

Value* read() {
  Token token, token2;

  while(true) {
    token = next_token();

    switch(token) {
      // Atomic values
      case TK_EXCEPTION:
      case TK_STRING:
      case TK_FIXNUM: return pop_token_value();
      case TK_EOF: return ODD_EOF;
      case TK_TRUE: return ODD_TRUE;
      case TK_FALSE: return ODD_FALSE;
      // Symbols and function calls
      case TK_SYMBOL: {
        Value* save_token = *token_value;
        // Must be framed because peek_token may allocate memory
        ODD_FRAME(save_token);
        token2 = peek_token(false);
        ODD_READ_CHECK_TK(token2);

        // Access
        if(token2 == TK_DOT) {
          save_token = read_access(save_token);
          ODD_CHECK(save_token);
          return save_token;
        }

        return check_function_application(save_token);
      }
      // List literals
      case TK_LBRACKET: {
        Token null_check = peek_token();
        if(null_check == TK_RBRACKET) {
          discard_lookahead();
          return ODD_NULL;
        }
        return read_list(LITERAL);
      }
      case TK_QUOTE:
      case TK_QUASIQUOTE:
      case TK_UNQUOTE:
      case TK_UNQUOTE_SPLICING: {
        State::Global g = State::S_QUOTE;
        switch(token) {
          case TK_QUASIQUOTE: g = State::S_QUASIQUOTE; break;
          case TK_UNQUOTE: g = State::S_UNQUOTE; break;
          case TK_UNQUOTE_SPLICING: g = State::S_UNQUOTE_SPLICING; break;
          default: break;
        }
        size_t line = Reader::line;

        Value *cell = NULL, *cell2 = NULL, *value = NULL, *symbol = NULL;
        ODD_FRAME(cell, value, symbol);

        value = read();
        ODD_CHECK(value);
        symbol = state.global_symbol(g);
        cell2 = state.cons(value, ODD_NULL);
        cell = state.cons(symbol, cell2, true);

        ODD_CAST(Pair, cell)->src.file = file;
        ODD_CAST(Pair, cell)->src.line = line;

        return cell;
      }
      // Stand-alone brace clauses
      case TK_LBRACE: {
        return read_list(BRACE_CLAUSE);
      }
      // Whitespace
      case TK_WHITESPACE: 
      case TK_NEWLINE: continue;
      // These values should not be encountered in the main loop
      case TK_RPAREN: return parse_error("unexpected ')'");
      case TK_RBRACE: return parse_error("unexpected '}'");
      case TK_LPAREN: return parse_error("unexpected '('");
      case TK_RBRACKET: return parse_error("unexpected ']'");
      case TK_DOT: return parse_error("unexpected '.'");
      case TK_LOOKAHEAD: ODD_ASSERT(false);
      default: {
        std::ostringstream os;
        os << "unexpected character '" << (char) token << '\'';
        return parse_error(os.str());
      }
                        std::cerr << "unexpected character " << (int)token << ' '  << (char)token << std::endl;
    }
    ODD_ASSERT(false);
  }
}

};

///// (CC) Compiler

// <--
// The Odd compiler is a simple, one-pass compiler. An instance of the
// compiler is created for each function (for the purposes of the compiler,
// something like a module source file is considered a function). There is
// no separate parsing step; the compiler operates directly on s-expressions.
// It is a little hairy in places because of this, but doing everything in
// one pass makes it much shorter overall.

// The compiler compiles a very small subset of Scheme. Most special forms,
// such as let, are implemented with macros.

// Perhaps the most complex machinery in the compiler is the handling of free
// variables (for Odd's purposes, a "free variable" is defined as any
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

// The core environment which contains special forms and builtins
Handle<Table> core_module;
// User interaction environment (eg REPL, one-off evaluations)
Handle<Table> user_module;

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

// An environment is a table

// Each environment has a special value #delegates with either a vector of tables (for modules) or a single table (for
// functions). This variable tells lookup where to search for variables if lookup in a particular table fails

// Entries in a module table are either 'special', a syntax transformer, or a symbol name indicating a global variable

// Entries in a function table are either syntax transformers or a fixnum indicating a local variable's index in the
// virtual machine's locals array

Table* make_env(Value* parent = ODD_FALSE, const char* module_name = 0) {
  Table* table = 0, *exports = 0;
  Vector* delegates = 0;
  String* name = 0;
  ODD_S_FRAME(parent, table, delegates, name, exports);
  table = make_table();
  // If this is a module
  if(module_name) {
    delegates = make_vector();
    if(parent) vector_append(delegates, parent);
    table_insert(table, global_symbol(S_UNQUALIFIED_IMPORTS), delegates); 
    delegates = make_vector();
    table_insert(table, global_symbol(S_QUALIFIED_IMPORTS), delegates);

    exports = make_table();

    table_insert(exports, global_symbol(S_GLOBAL_ENVIRONMENT), ODD_TRUE);
    table_insert(exports, global_symbol(S_EXPORT_PARENT), table);

    table_insert(table, global_symbol(S_EXPORTS), exports);

    name = make_string(module_name);
    table_insert(table, global_symbol(S_MODULE_NAME), name);

    table_insert(table, global_symbol(S_GLOBAL_ENVIRONMENT), ODD_TRUE);
    table_insert(table, global_symbol(S_EXPORTING), ODD_FALSE);
  } else {
    table_insert(table, global_symbol(S_PARENT), parent);
  }
  return table;
}

bool env_contains(Table* env, Value* key) {
  return table_contains(env, key);
}

void env_define(Table* table, Value* key, Value* value, bool exported = false) {
  Value* chk = 0;
  Table* exports = 0;
  if(exported) exports = ODD_CAST(Table, table_get(table, global_symbol(S_EXPORTS)));
  ODD_S_FRAME(table, key, value, chk, exports);
  if(table_contains(table, key)) {
    chk = table_set(table, key, value);
    if(exported) table_set(exports, key, value);
  } else {
    chk = table_insert(table, key, value);
    if(exported) {
      if(!table_has_key(exports, key)) {      
        table_insert(exports, key, value);
      } else {
        table_set(exports, key, value);
      }
    }
  }
  ODD_ASSERT(!chk->active_exception());
}

// Whether variables in this environment are global or not
bool env_globalp(Table* table) {
  return table_has_key(table, global_symbol(S_GLOBAL_ENVIRONMENT));
}

// Look up an environment variable (the result is rather complex, and
// returned in a structure)

enum RefScope { REF_GLOBAL, REF_UP, REF_LOCAL };
struct Lookup {
  Lookup(): scope(REF_LOCAL), success(true) {
    // Make sure everything is zero-initialized
    nonglobal.index = nonglobal.level = 0;
    nonglobal.value = 0;

    global.value = 0;
    global.module = 0;
  }

  RefScope scope;
  bool success;
  union {
    struct { Value* value; Table* module; } global; 
    struct { unsigned index, level; Value* value; } nonglobal;
  };
};

// Determines whether a lookup result refers to a special form or macro
bool lookup_syntaxp(Lookup& lookup) {
  if(lookup.success) {
    if(lookup.scope == REF_GLOBAL) {
      return lookup.global.value == global_symbol(S_SPECIAL) ||
             lookup.global.value->applicablep();
    } else {
      return lookup.nonglobal.value->applicablep();
    }
  }
  return false;
}

Value* env_qualify_name(Table* env, Value* name) {
  bool found;
  Value* module_name = table_get(env, global_symbol(S_MODULE_NAME), found);
  ODD_ASSERT(found);
  std::ostringstream ss;
  ss << module_name->string_data() << '#' << name;

  return make_symbol(ss.str());
}

void env_lookup(Table* env, Value* key, Lookup& lookup, bool search_exports = false) {
  bool found = false;
  Table* start_env = env;
  // Search through environments
  while(env->get_type() == TABLE) {
    // Search through an environment
    Value* cell = table_get_cell(env, key);
    if(cell != ODD_FALSE) {
      // Found a match -- determine scope
      ODD_ASSERT(cell->get_type() == PAIR);
      if(env_globalp(env)) {
        lookup.scope = REF_GLOBAL;
        lookup.global.value = cell->cdr();
        lookup.global.module = env;
        if(search_exports) lookup.global.module = (Table*)table_get(env, global_symbol(S_EXPORT_PARENT));
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
    // Nothing in this environment, keep searching

    // Check whether this environment is a child environment (such as a function within a module)
    if(table_has_key(env, global_symbol(S_PARENT))) {
      env = ODD_CAST(Table, table_get(env, global_symbol(S_PARENT), found));
      ODD_ASSERT(found);
      continue;
    }
    // No delegates or delegate search failed, we're done
    break;
  }

  // Check for unqualified imports
  if(table_has_key(env, global_symbol(S_UNQUALIFIED_IMPORTS))) {
    Vector* modules = ODD_CAST(Vector, table_get(env, global_symbol(S_UNQUALIFIED_IMPORTS), found));
    for(size_t i = modules->vector_length(); i--;) {
      Lookup sublookup;
      Table* exports = ODD_CAST(Table, table_get(ODD_CAST(Table, modules->vector_ref(i)), global_symbol(S_EXPORTS)));

      env_lookup(exports, key, sublookup, true);

      if(sublookup.success) {
        lookup = sublookup;
        return;
      }
    }
  }

  lookup.success = false;
}

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

// An instance of the compiler, created for each function or module
struct Compiler {
  Compiler(State& state_, Compiler* parent_, Table* env_ = NULL, size_t depth_ = 0): 
    state(state_), parent(parent_),
    local_free_variable_count(0), upvalue_count(0),
    stack_max(0), stack_size(0), locals(0), constants(state), closure(false),
    name(state_), env(state_), synclo_env(state_), synclo_fv(state_), escaped_variables(state_), depth(depth_), last_insn(0) {

    if(!env_) env = (*state.core_module);
    else env = env_;

    if(parent) escaped_variables = (*parent->escaped_variables);
    else {
      name = state.make_string("<toplevel>");
    }
  }

  ~Compiler() {}

  State& state;
  // The parent function -- necessary for free variables
  Compiler* parent;
  // The wordcode
  std::vector<size_t> code;
  // A vector containing combinations of instruction offsets and source
  // location info for debugging and tracebacks
  std::vector<debug_t> debuginfo;
  // A vector containing free variable locations
  std::vector<FreeVariableInfo> free_variables;
  unsigned local_free_variable_count, upvalue_count;

  unsigned stack_max, stack_size, locals;
  Handle<Vector> constants;
  bool closure;
  // The name of the function (if any)
  Handle<String> name;
  // The environment of the function
  Handle<Table> env;
  // These variables exist only when compiling a syntactic closure. 

  // synclo_env is the definition environment of the syntactic closure; it is searched for free variables when they are
  // encountered
  Handle<Table> synclo_env;
  // synclo_fv is a list of symbols introduced by syntactic closures
  Handle<Pair> synclo_fv;
  // Escaped variables (for macros)
  Handle<Vector> escaped_variables;
  // The depth of the function (for debug message purposes)
  size_t depth;
  // Location of last instruction emitted (for debug message purposes)
  size_t last_insn;

  // Basic code generation
  void emit(size_t word) {
    last_insn = code.size();
    code.push_back(word);
  }

  void emit_arg(size_t word) {
    code.push_back(word);
  }

  void emit_pop() {
    ODD_CC_EMIT("pop");
    emit(OP_POP);
    pop();
  }

  // In order to push a constant onto the stack, we have to save it and store
  // it along with the function. This function lazily creates the vector of
  // constants and makes sure no constants are repeated.
  void push_constant(Value* constant) {
    push();
    Vector* cs = *constants;
    ODD_FRAME(cs, constant);
    // Lazily create constant vector
    if(cs == ODD_FALSE) {
      cs = state.make_vector();
      constants = cs;
    }
    // Check for existing constant
    for(unsigned i = 0; i != cs->vector_length(); i++) {
      if(cs->vector_ref(i) == constant) {
        // Constant already exists, don't append it to vector
        emit(OP_PUSH_CONSTANT);
        emit_arg(i);
        ODD_CC_VMSG("push-constant " << constant << " " << i <<
                    " (existing constant)");
        return;
      }
    }
    // Constant does not exist, add to vector and then push
    unsigned i = state.vector_append(cs, constant);
    emit(OP_PUSH_CONSTANT);
    emit_arg(i);
    ODD_CC_EMIT("push-constant " << constant << " " << i);
  }

  // Stack size management; determines how much memory will be allocated for
  // the stack
  void push(ptrdiff_t i = 1) {
    stack_size += i;
    if(stack_size > stack_max) stack_max = stack_size;
    ODD_CC_VMSG("PUSH: stack_size = " << stack_size);
  }

  void pop(ptrdiff_t i = 1) {
    stack_size -= i;
    // Ensure we don't go below 0
    ODD_CC_VMSG("POP: stack_size = " << stack_size);
    ODD_ASSERT(stack_size < (unsigned) 0-5);
  }

  // Annotating virtual machine instructions with their source location for
  // debug purposes
  void annotate(Value* exp) {
    SourceInfo src;
    unwrap_source_info(exp, src);
    if(src.file) {
      unsigned insn = code.size() ? (code.size() - 1) : 0;
      debug_t dbg(insn, src);
      debuginfo.push_back(dbg);
      ODD_CC_MSG("annotate " << insn << ' ' << dbg.second.file << ':' << dbg.second.line);
    }
  }

  // Error handling
  Value* syntax_error(Value* form, const std::string& str) {
    std::string out;
    state.format_source_error(form, str, out);
    return state.make_exception(State::S_ODD_COMPILE, out);
  }

  Value* arity_error(Global name, Value* form, size_t expected, unsigned got) {
    std::ostringstream ss;
    ss << "malformed " << state.global_symbol(name) << ": expected "
       << expected << " arguments, but got " << got;
    return syntax_error(form, ss.str());
  }

  Value* arity_error_least(Global name, Value* form, size_t expected,
                          unsigned got) {
    std::ostringstream ss;
    ss << "malformed " << state.global_symbol(name) << ": expected at least "
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
    return state.make_exception(State::S_ODD_COMPILE, out);
  }

  bool exporting() { return state.env_globalp(*env) && state.table_get(*env, state.global_symbol(S_EXPORTING)); }

  // Checks a function to see whether it is a call to a particular special
  // form
  bool check_special_form(Value* form, Global g) {
    Table* check_env = *env;
    // Quick check to make sure it has the right format and name
    if(form->get_type() != PAIR && form->get_type() != SYNCLO) return false;
    if(form->get_type() == SYNCLO) {
      Value* sync = form;
      form = form->synclo_expr();
      if(form->get_type() != PAIR) return false;
      check_env = sync->synclo_env();
    }
    Value* check = unbox(form->car());
    if(check->get_type() != SYMBOL || check != state.global_symbol(g))
      return false;

    // But we still need to do a real lookup because form names could be
    // shadowed by other definitions
    Lookup lookup;
    state.env_lookup(check_env, check, lookup);
    return lookup.success && lookup.scope == REF_GLOBAL &&
      lookup.global.value == state.global_symbol(S_SPECIAL);
  }

  bool identifierp(Value* x) {
    return x->get_type() == SYMBOL || (x->get_type() == BOX && unbox(x)->get_type() == SYMBOL) ||
      (x->get_type() == SYNCLO && (static_cast<Synclo*>(x)->expr->get_type() == SYMBOL));
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
    ODD_CC_VMSG("register free variable <" << l.lexical_level << ", "
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

  Value* generate_ref(Lookup& lookup, Value* exp,  Value* name) {
    switch(lookup.scope) {
      case REF_GLOBAL: {
        if(lookup.global.value == state.global_symbol(S_SPECIAL)) {
          std::ostringstream ss;
          ss << "attempt to use special form '" << exp << "' as a value";
          return syntax_error(exp, ss.str());
        }
        if(lookup.global.value->applicablep())
          return syntax_error(exp, "macros cannot be used as values");

        //push_constant(lookup.global.value);
        Table *module = lookup.global.module;
        Value* tmp = 0;
        ODD_FRAME(exp, name, module, tmp);
        tmp = state.env_qualify_name(module, name);

        push_constant(tmp);

        emit(OP_GLOBAL_GET);
        ODD_CC_EMIT("global-get " << tmp);
        // No stack change -- the name will be popped, but the value will be
        // pushed       
        break;
      }
      case REF_UP:
      case REF_LOCAL: {
        // Make sure we haven't been passed a macro (there can be local
        // macros in the case of let-syntax)
        if(lookup.nonglobal.value->applicablep()) 
          return syntax_error(exp, "macros cannot be used as values");
        if(lookup.scope == REF_LOCAL) {
          emit(OP_LOCAL_GET);
          emit_arg(lookup.nonglobal.index);
          ODD_CC_EMIT("local-get " << name << ' ' << lookup.nonglobal.index);
          push();
        } else {
          ODD_ASSERT(lookup.scope == REF_UP);
          unsigned index = register_free_variable(lookup);
          emit(OP_UPVALUE_GET);
          emit_arg(index);
          ODD_CC_EMIT("upvalue-get " << name << ' ' << index);
          push();
        }
        break;
      }
    }
    return ODD_FALSE;
  }

  // Compile a variable reference
  Value* compile_ref(Value* exp) {
    annotate(exp);
    Value* name = unbox(exp);
    Lookup lookup;
    state.env_lookup(*env, name, lookup);
    if(!lookup.success) return undefined_variable(exp);
    return generate_ref(lookup, exp, name);
  }

  // Generate a set (used by both define and set)
  // Assumes value is already on the stack
  void generate_set(Table* env, Lookup& lookup, Value* name) {
    switch(lookup.scope) {
      case REF_GLOBAL: {
        // The global-set must be qualified with the module's name, for instance if we're in the module "user" and have
        // a variable "x", it should become #user#x

        // Retrieve the module's name
        Value* qualified_name = 0;
        ODD_FRAME(env, name, qualified_name);
        qualified_name = state.env_qualify_name(env, name);

        push_constant(qualified_name);
        emit(OP_GLOBAL_SET);
        ODD_CC_EMIT("global-set " << name);
        // Note we only pop once here to compensate for the push_constant; there is another pop generated after every
        // set
        pop();
        break;
      }
      case REF_LOCAL:
        emit(OP_LOCAL_SET);
        emit_arg(lookup.nonglobal.index);
        ODD_CC_EMIT("local-set " << (unsigned) lookup.nonglobal.index);
        break;
      case REF_UP: {
        unsigned index = register_free_variable(lookup);
        emit(OP_UPVALUE_SET);
        emit_arg(index);
        ODD_CC_EMIT("upvalue-set " << name << ' ' << index);
        break;
      }
    }
    pop();
  }

  // Special forms
  Value* compile_define(size_t argc, Value* exp, bool tail) {
    // There's a goto here (yuck) because we'll restart the compilation
    // process if this define is a function definition
restart:
    Value* chk = 0;
    if(argc != 2) return arity_error(S_DEF, exp, 2, argc);
    Value* name = unbox(exp->cadr());
    Value* args = 0;

    // Parse a define lambda expression, e.g.: 
    // (define name (lambda () #t))
    // becomes
    // (define name (#named-lambda name () #t)
    Value* test = unbox(exp->caddr());
    if(test->get_type() == PAIR) {
      if(check_special_form(test, S_LAMBDA)) {
        // Yep, start all over again
        Value *named_lambda = 0;
        ODD_FRAME(exp, name, args, named_lambda, test);
        // Get the rest of the expression
        named_lambda = state.cons(name, test->cdr());
        named_lambda = state.cons(state.global_symbol(S_NAMED_LAMBDA), named_lambda);
        named_lambda = state.cons_source(named_lambda, ODD_NULL, exp);
        exp->cdr()->set_cdr(named_lambda);
        goto restart;
      }
    }
    // TODO: Handle (define name (lambda () #t))

    // We'll create a fake lookup structure for generate_set here
    Lookup lookup;
    Value* value = 0;
    if(state.env_globalp(*env)) {
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
    state.env_define(*env, name, value, exporting());

    // Compile body for set
    ODD_FRAME(name);
    chk = compile(exp->cddr()->car());
    ODD_CHECK(chk);
    // Set variable
    generate_set(*env, lookup, name);
    return ODD_FALSE;
  }

  Value* compile_set(size_t argc, Value* exp, bool tail) {
    Value* chk = 0;
    // set always has two arguments
    if(argc != 2) return arity_error(S_SET, exp, 2, argc);
    Value* name = unbox(exp->cadr());
    // Check that name is a symbol
    if(name->get_type() != SYMBOL)
      return syntax_error(S_SET, exp,
                          "first argument to set must be a symbol");

    // Compile body 
    chk = compile(exp->cddr()->car());
    ODD_CHECK(chk);

    // Look up variable
    Lookup lookup;
    state.env_lookup(*env, name, lookup);
    // Don't allow special forms or syntax
    if(!lookup.success)
      return undefined_variable(exp->cadr());
    if(state.lookup_syntaxp(lookup))
      return syntax_error(exp, "syntax cannot be set like a variable");

    generate_set(*env, lookup, name);

    // note pop
    return ODD_FALSE;
  }

  Value* compile_brace(size_t argc, Value* exp, bool tail) {
    if(argc == 0) {
      return compile(ODD_UNSPECIFIED, tail);
    } else {
      Value* body = exp->cdr();
      Value* elt = 0, *chk = 0;
      ODD_FRAME(body, elt, chk);
      while(body != ODD_NULL) {
        elt = body->car();
        if(body->cdr() == ODD_NULL) {
          return compile(elt, tail);
        } else {
          size_t stack_chk = stack_size;
          chk = compile(elt, false);
          ODD_CHECK(chk);
          body = body->cdr();
          // Pop result, if expression left anything on the stack
          // (which may not happen if e.g. there is a set or define)
          if(stack_size > stack_chk) emit_pop(); 
        }
      }
    }
    return ODD_FALSE; // should never be reached
  }

  Value* generate_lambda(size_t form_argc, Value* exp, bool tail, Value* name, bool& closure) {
    // For checking subcompilation results
    Value* chk = 0;
    if(form_argc < 1) return arity_error(S_LAMBDA, exp, 1, form_argc);
    // Create new compiler, environment
    unsigned argc = 0;
    // Will be set to true if the function is variable arity
    // (i.e. takes unlimited arguments)
    bool variable_arity = false;
    Table *new_env = 0;
    Value *args = 0, *body = 0, *arg = 0;
    Prototype* proto = 0;
    ODD_FRAME(exp, name, new_env, args, arg, body, proto);

    // Create a new environment for the function with the current environment
    // as its parent
    new_env = state.make_env(*env);
    // Make the current environment its parent environment
    // state.vector_append(ODD_CAST(Vector, new_env), *env);
    // Now parse the function's arguments and define them within the
    // new environment
    args = exp->cdr();
    if(args != ODD_NULL) {
      if(args->get_type() != PAIR) 
        return syntax_error(S_LAMBDA, exp, "first argument to lambda must be either () or a list of arguments");
      while(args->get_type() == PAIR) {
        arg = unbox(args->car());
        // This is bad. I don't like this special case for synclos.
        if(arg->get_type() == SYNCLO) {
          if(check_special_form(arg, S_BRACE)) {
            if(args->cdr() != ODD_NULL)
              return syntax_error(S_LAMBDA, exp, "additional arguments to lambda after body");
            break;
          } else {
            variable_arity = true;
          }
          break;
        }
        if(arg->get_type() == PAIR) {
          if(check_special_form(arg, S_BRACE)) {
            // Encountered lambda body, stop parsing
            if(args->cdr() != ODD_NULL)
              return syntax_error(S_LAMBDA, exp, "additional arguments to lambda after body");
            break;
          } else {
            variable_arity = true;
          }
          break;
        }
        if(arg->get_type() != SYMBOL) {
          if(arg->get_type() == PAIR && variable_arity)
            return syntax_error(S_LAMBDA, exp, "multiple variable arity arguments to lambda");
          return syntax_error(S_LAMBDA, exp, "lambda argument list must be symbols");
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
    ODD_CC_MSG("compiling subfunction");
    Compiler cc(state, this, new_env, depth+1);
    cc.locals = argc;
    if(name) {
      cc.name = static_cast<String*>(name->symbol_name());
    }
    body = arg;
    chk = cc.compile(body, true);
    ODD_CHECK(chk);

    proto = cc.end(argc, variable_arity);
    closure = cc.closure;
    return proto;
  }

  Value* compile_lambda(size_t form_argc, Value* exp, bool tail, Value* name) {
    bool closure = false;
    Value* proto = generate_lambda(form_argc, exp, tail, name, closure);
    if(proto->active_exception()) return proto;
    push_constant(proto);
    // If the function's a closure, we'll have to close over it
    if(closure) {
      ODD_CC_EMIT("close-over");
      emit(OP_CLOSE_OVER);
    }
    return ODD_FALSE;
  }

  Value* compile_if(size_t argc, Value* exp, bool tail) {
    if(argc != 2 && argc != 3) {
      if(argc < 2) return arity_error(S_IF, exp, 2, argc);
      else return arity_error_most(S_IF, exp, 3, argc);
    }
    Value* chk = 0, *condition = 0, *then_branch = 0, *else_branch = 0;
    ODD_FRAME(exp, chk, condition, then_branch, else_branch);

    condition = exp->cadr();
    then_branch = exp->cddr()->car();
    else_branch = argc == 2 ? ODD_UNSPECIFIED : exp->cddr()->cdr()->car();

    // Compiling ifs is actually pretty simple. We push the condition onto the stack and then place jumps in the code.
    // If the condition is false, we'll skip the "then" branch. At the end of the "then" branch, we'll skip the "else"
    // branch, so if the condition was true and the "then" branch was executed, we skip it.

    // Since we don't know exactly where to jump to, what we do is emit the jump instructions with arguments of 0, then
    // change them after we know where to jump to.

    chk = compile(condition);
    ODD_CHECK(chk);
    // Jump to the else branch if the condition is false
    emit(OP_JUMP_IF_FALSE);
    // Emit placeholder for argument
    size_t jmp1 = code.size();
    emit_arg(0);
    // JMP_IF_FALSE will pop condition
    pop();
    ODD_CC_EMIT("jump-if-false");
    size_t old_stack = stack_size;
    // Emit 'then' code
    chk = compile(then_branch, tail);
    ODD_CHECK(chk);
    pop(stack_size - old_stack);
    // Jmp to the rest of the program
    emit(OP_JUMP);
    // Emit placeholder for argument
    size_t jmp2 = code.size();
    emit_arg(0);
    ODD_CC_EMIT("jump");
    // Location of first jump (beginning of else branch)
    size_t lbl1 = code.size();
    chk = compile(else_branch, tail);
    ODD_CHECK(chk);
    pop(stack_size - old_stack);
    // Location of second jump
    size_t lbl2 = code.size();

    // Replace jumps
    code[jmp1] = lbl1;
    code[jmp2] = lbl2;

    // Ensure space for result
    push();

    ODD_CC_VMSG("if jmp1 " << lbl1 << " jmp2 " << lbl2);

    return ODD_FALSE;
  }
  
  // For quote: strip syntactic closures and boxes, neither of which should appear in end-user code.
  Value* strip_syntax(Value* exp) {
    Value *car=0, *cdr=0;
    ODD_FRAME(exp, car, cdr);
recur:
    switch(exp->get_type()) {
      case SYNCLO:
        exp = exp->synclo_expr();
        goto recur;
      case BOX:
        exp = exp->box_value();
        goto recur;
      case PAIR:
        car = strip_syntax(exp->car());
        cdr = strip_syntax(exp->cdr());
        exp = state.cons_source(car, cdr, exp);
        return exp;
      default: return exp;
    }
  }

  Value* compile_quote(size_t argc, Value* exp) {
    if(argc != 1) return arity_error(S_QUOTE, exp, 1, argc);
    // Immediates are stored directly in code
    if(exp->cadr()->immediatep()) {
      return compile(exp->cadr());
    } else {
      // Everything else is a constant
      Value* x = strip_syntax(exp->cadr());
      push_constant(x);
      return ODD_FALSE;
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
    ODD_FRAME(exp, chk, args);
    while(args != ODD_NULL) {
      chk = compile(args->car());
      ODD_CHECK(chk);
      args = args->cdr();
    }

    size_t argc = stack_size - old_stack_size;
    // Compile the actual function, which might be a symbol or perhaps
    // an inline anonymous function (e.g. ((lambda () #t)))
    chk = compile(exp->car());
    ODD_CHECK(chk);

    emit(tail ? OP_TAIL_APPLY : OP_APPLY);
    ODD_ASSERT(argc < 255);
    emit_arg((unsigned char) argc); 
    ODD_CC_EMIT((tail ? "tail-apply" : "apply") << ' ' << argc);
    // The application will pop the arguments and the function once finished,
    // but will push a result
    pop(stack_size - old_stack_size);
    push();
    return ODD_FALSE;
  }

  Value* compile_define_syntax(size_t argc, Value* exp, bool tail) {
    if(argc != 4) return arity_error(S_DEFSYNTAX, exp, 4, argc);
    // Macro name
    Value* name = unbox(exp->list_ref(1));
    // Argument names
    Value* expr_name = unbox(exp->list_ref(2));
    Value* env_name = unbox(exp->list_ref(3));
    Value* body = unbox(exp->list_ref(4));

    if(name->get_type() != SYMBOL || expr_name->get_type() != SYMBOL || env_name->get_type() != SYMBOL)
      return syntax_error(exp, "first three arguments to defsyntax must be symbols");

    Value* transformer = 0, *swap = 0, *lambda = 0, *apply_macro_fn = 0;
    VectorStorage* apply_macro_closure = 0;
    ODD_FRAME(exp, name, expr_name, env_name, transformer, swap, lambda, apply_macro_fn, apply_macro_closure);

    // Create transformer function expression
    swap = state.cons(body, ODD_NULL);
    swap = state.cons(env_name, swap);
    swap = state.cons(expr_name, swap);
    swap = state.cons(ODD_FALSE, swap);

    // Compile transformer function
    bool closure;
    transformer = generate_lambda(3, swap, tail, name, closure);
    //ODD_ASSERT(!closure);
    ODD_CHECK(transformer);
    ODD_ASSERT(transformer->applicablep());

    // Create a closure with apply-macro, the transformer function and the current compilation environment
    apply_macro_closure = state.vector_storage_realloc(2, 0);
    apply_macro_closure->data[0] = *env;
    apply_macro_closure->data[1] = transformer;
    apply_macro_closure->length = 2;
    apply_macro_fn = state.make_native_function(&apply_macro, 0, true, apply_macro_closure);
    
    // Define macro
    state.env_define(*env, name, apply_macro_fn, exporting());
    ODD_CC_MSG("defsyntax " << name << ' ' << transformer << ' ' << state.table_get(*env, state.global_symbol(S_MODULE_NAME)));
    return ODD_FALSE;
  }

  Value* compile_macro_application(size_t argc, Value* exp, bool tail, Lookup& lookup) {
    // Syntax
    state.safestack.clear();
    // First argument is the expression
    state.safestack.push_back(strip_syntax(exp));
    // Second argument is the usage environment of the macro, for creating syntactic closures with.
    state.safestack.push_back(*env);

    // Get the actual transformer
    Value* function = lookup.scope == REF_GLOBAL ? lookup.global.value : lookup.nonglobal.value;
    Value* result = 0;
    Value* other_env = 0;

    ODD_CC_MSG("macro application " << exp);
    // Run the transformer
    result = state.apply(function, state.safestack.size(), &state.safestack[0]);

    ODD_CHECK(result);
    
    // Here's the fun part, we extract the definition environment of the macro (which is part of the transformer's closure) 
    other_env = static_cast<NativeFunction*>(function)->closure->data[0];

    // Swap it with the current environment
    env.swap(other_env);

    ODD_CC_MSG("macro expansion result " << result);
    // Compile the macro's return value in the definition environment of the macro
    result = compile(result, tail);

    // Then swap back
    env.swap(other_env);

    return result;
  }

  // Switch to module
  
  // Can be called either by an explicit module definition, or implicitly by load_module
  Value* enter_module(Value* internal_name) {
    ODD_CC_MSG("entering module " << internal_name);
    Table* module_env = 0;
    Value* chk = 0;
    ODD_FRAME(internal_name, module_env, chk);

    if(state.module_loaded(ODD_CAST(String, internal_name))) {
      env = ODD_CAST(Table, state.table_get(*state.modules, internal_name));

      return ODD_FALSE;
    }

    // Create new module table
    module_env = state.make_env(*state.core_module, internal_name->string_data());

    // Register module
    chk = state.register_module(ODD_CAST(String, internal_name), module_env);
    ODD_CHECK(chk);

    // Change compilers module
    env = module_env;
    
    return ODD_FALSE; 
  }

  // Handle a module definition
  Value* module(size_t argc, Value* exp) {
    // module must have at least a module name and specification
    if(argc != 1) return arity_error(S_MODULE, exp, 2, argc);
    Value* name = exp->cadr();
    // Parse module name
    if(name->get_type() != PAIR && name->get_type() != SYMBOL)
      return syntax_error(exp, "first argument to module must be a "
                          "valid module name (a symbol or a list of one or more symbols)");
    Value* internal_name = 0;

    ODD_FRAME(name, exp, internal_name);

    // Convert module name into internal format
    // (scheme base) => #scheme#base
    internal_name = state.convert_module_name(name);
    ODD_CHECK(internal_name);

    return enter_module(internal_name);
  }

  // Resolve an access statement (called both when an "access" statement is encountered in plain code and when it's
  // applied (eg 'module.macro()')
  Value* resolve_access(Value* exp, Value* args, Table*& modref, Value *& nameref) {
    Value* qualified_imports = state.table_get(*env, state.global_symbol(S_QUALIFIED_IMPORTS));
    Value* module_name = 0;
    Value* variable_name = 0, *gross_hack = 0;
    Value* tmp = 0;
    Table* module = 0;

    ODD_FRAME(exp, args, qualified_imports, module_name, variable_name, tmp, module, gross_hack);

    module_name = tmp = args;

    while(args->get_type() == PAIR) {
      if(args->cdr() == ODD_NULL) {
        variable_name = unbox(args->car());
        gross_hack = args;
        tmp->set_cdr(ODD_NULL);
        break;
      }
      tmp = args;
      args = args->cdr();
    }

    module_name = state.convert_module_name(module_name);
    tmp->set_cdr(gross_hack);

    for(size_t i = 0; i != qualified_imports->vector_length(); i++) {
      if(equals(module_name, state.table_get(ODD_CAST(Table, qualified_imports->vector_ref(i)), state.global_symbol(S_MODULE_NAME)))) {
        module = ODD_CAST(Table, qualified_imports->vector_ref(i));
      }
    }

    if(!module) {
      std::ostringstream ss;
      ss << "attempt to access unimported module " << module_name;
      return syntax_error(exp, ss.str());
    }

    modref = module;
    nameref = variable_name;

    return ODD_FALSE;
  }

  Value* lookup_access_ref(size_t argc, Value* exp, Value*& nameref, Lookup& lookupref) {
    Value* variable_name = 0;
    Table* module = 0;
    Value* chk = resolve_access(exp, exp->cdr(), module, variable_name);
    ODD_CHECK(chk);

    Lookup lookup;
    state.env_lookup(module, variable_name, lookup);
    // TODO: Improve error message
    if(!lookup.success) {
      String* modname = ODD_CAST(String, state.table_get(module, state.global_symbol(S_MODULE_NAME)));
      std::ostringstream ss;
      ss << "failed to resolve name '" << variable_name << "' in module '" << write_module_name(modname) << "'";
      return syntax_error(exp, ss.str());
    }
    
    lookupref = lookup;
    nameref = variable_name;
    return ODD_FALSE;
  }

  Value* compile_access_ref(size_t argc, Value* exp) {
    if(argc < 2) return arity_error_least(S_ACCESS, exp, 2, argc);

    Lookup lookup;
    Value* name = 0;
    Value* chk = lookup_access_ref(argc, exp, name, lookup);
    ODD_CHECK(chk);

    return generate_ref(lookup, exp, name);    
  }

  // Handle an import statement
  Value* import(size_t argc, Value* exp) {
    if(argc != 1) return arity_error(S_IMPORT, exp, 2, argc);
    //(car (car (cdr x)))
    if(exp->cadr()->get_type() != PAIR || exp->caadr() != state.global_symbol(S_BRACE))
      return syntax_error(exp, "import must be followed by a brace clause");
    
    Value* modules = exp->cdadr();
    Value* clause = 0;
    Value* name = 0;
    Vector* qualified_imports = 0;
    Vector* unqualified_imports = 0;
    Value* module = 0;
    
    ODD_FRAME(exp, modules, clause, name, qualified_imports, unqualified_imports, module);

    qualified_imports = ODD_CAST(Vector, state.table_get(*env, state.global_symbol(S_QUALIFIED_IMPORTS)));
    unqualified_imports = ODD_CAST(Vector, state.table_get(*env, state.global_symbol(S_UNQUALIFIED_IMPORTS)));

    while(modules->get_type() == PAIR) {
      Value* clause = unbox(modules->car());

      if(clause->get_type() != SYMBOL && clause->get_type() != PAIR &&
         !(clause->get_type() == PAIR && clause->car() != state.global_symbol(S_ACCESS))) {
        std::ostringstream ss;
        ss << "import arguments must be either symbols or accesses (eg odd or odd.core) but got " << clause;
        return syntax_error(exp, ss.str());
      }
      
      bool unqualified = false;
      Value* tmp = clause;
      while(tmp->get_type() == PAIR) {
        if(tmp->cdr() != ODD_NULL && unbox(tmp->cadr()) == state.global_symbol(S_ASTERISK)) {
          unqualified = true;
          tmp->set_cdr(ODD_NULL);
        }
        tmp = tmp->cdr();
      }

      name = clause->get_type() == PAIR ? state.convert_module_name(clause->cdr()) : state.convert_module_name(clause);

      module = state.load_module(ODD_CAST(String, name));
      ODD_CHECK(module);
      state.vector_append(unqualified ? unqualified_imports : qualified_imports, module);

      modules = modules->cdr();
    }

    return ODD_FALSE;
  }

  Value* compile_export(size_t argc, Value* exp) {
    Value* lst = exp->cdr();
    Value* name = 0;
    Table* exports = 0;
    ODD_FRAME(lst, name, exp, exports);
    exports = ODD_CAST(Table, state.table_get(*env, state.global_symbol(S_EXPORTS)));
    while(lst->get_type() == PAIR) {
      name = unbox(lst->car());
      if(name->get_type() == SYMBOL) {
        if(state.table_has_key(exports, name)) continue;
        state.table_insert(exports, name, name);
      }
      lst = lst->cdr();
    }
    return ODD_FALSE;
  }

  // Handle an export statement

  // Compile a single expression, returns #f or an error if one occurred
  Value* compile(Value* exp, bool tail = false) {
    ODD_CC_MSG("compile expression " << exp);
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
        ODD_CC_EMIT("push-immediate " << exp);
        break;
      // Non-immediate but literal constants
      case STRING:
        push_constant(exp);
        ODD_CC_EMIT("push-constant " << exp);
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
          state.env_lookup(*env, function, lookup);
          syntax = state.lookup_syntaxp(lookup);
        }
        // Special case: access macro application
        if(check_special_form(function, S_ACCESS)) {
            Value* name = 0;
            Value* chk = lookup_access_ref(length(function->cdr()), function, name, lookup);
            // If this is a macro application, do that instead
            if(state.lookup_syntaxp(lookup) && lookup.global.value != state.global_symbol(S_SPECIAL)) {
              return compile_macro_application(length(exp->cdr()), exp, tail, lookup);
            }
            ODD_CHECK(chk);
            return ODD_FALSE;
        }
        if(syntax) {
          size_t argc = length(exp->cdr());
          if(lookup.global.value == state.global_symbol(S_SPECIAL)) {
            // Dispatch on the special form
            Symbol* op = ODD_CAST(Symbol, function);
            // define
            if(op == state.global_symbol(S_DEF)) {
              return compile_define(argc, exp, tail);
            // set
            } else if(op == state.global_symbol(S_SET)) {
              return compile_set(argc, exp, tail);
            // brace
            } else if(op == state.global_symbol(S_BRACE)) {
              return compile_brace(argc, exp, tail);
            // lambda
            } else if(op == state.global_symbol(S_LAMBDA)) {
              return compile_lambda(argc, exp, tail, ODD_FALSE);
            // modules
            // public & private
            } else if(op == state.global_symbol(S_PUBLIC)) {
              state.table_set(*env, state.global_symbol(S_EXPORTING), ODD_TRUE);
              return ODD_FALSE;
            } else if(op == state.global_symbol(S_PRIVATE)) {
              state.table_set(*env, state.global_symbol(S_EXPORTING), ODD_FALSE);
              return ODD_FALSE;
            } else if(op == state.global_symbol(S_EXPORT)) {
              return compile_export(argc, exp);
            // access
            } else if(op == state.global_symbol(S_ACCESS)) {
              return compile_access_ref(argc, exp);
            // import
            } else if(op == state.global_symbol(S_IMPORT)) {
              return import(argc, exp);
            // named-lambda
            } else if(op == state.global_symbol(S_NAMED_LAMBDA)) {  
              // This is a little hacky, but we're going to extract the name
              // here and then make it look like a normal lambda for
              // compile_lambda
              
              // No need for parsing because named lambdas are generated by
              // the parser
              Value* name = unbox(exp->cadr());
              Value* rest = exp->cddr();
              exp->set_cdr(rest);

              return compile_lambda(argc-1, exp, tail, name);
            } else if(op == state.global_symbol(S_IF)) {
              return compile_if(argc, exp, tail);
            } else if(op == state.global_symbol(S_QUOTE)) {
              return compile_quote(argc, exp);
            } else if(op == state.global_symbol(S_DEFSYNTAX)) {
              return compile_define_syntax(argc, exp, tail);
            } else if(op == state.global_symbol(S_MODULE)) {
              return module(argc, exp);
            } else assert(0);
          } else {
            ODD_CC_MSG("applying macro " << function);
            return compile_macro_application(argc, exp, tail, lookup);
          }
        } else {
          // function application
          return compile_apply(exp, tail);
        }
        ODD_ASSERT(0);
      }
      case SYNCLO: {
        Synclo *s = static_cast<Synclo*>(exp);
        Value* senv = s->env;
        Value* expr = s->expr;
        Table* swap = 0;
        Value* chk = 0;
        Value* vars = 0;

        bool swapped = false;
        ODD_FRAME(s, senv, vars, expr, swap, chk);

        vars = state.vector_combine(s->escaped_variables, *escaped_variables);

        escaped_variables.swap(vars);

#if 0
        env.swap(senv);
        chk = compile(expr, tail);
        env.swap(senv);
#endif

        // If this is not a reference to an escaped variable, we'll use the synclo's specified environment
        if(!(identifierp(expr) && escaped_variables->vector_memq(expr))) {
          env.swap(senv);
          swapped = true;
        }

        chk = compile(expr, tail);

        if(swapped) {
          env.swap(senv);
        }

        escaped_variables.swap(vars);

        return chk;
      }
      default:
        std::cerr << exp << std::endl;
        ODD_ASSERT(!"unknown expression given to compiler");
        break;
    }
    return ODD_FALSE;
  }

  // Creates the actual Prototype at the end of compilation
  Prototype* end(unsigned arguments = 0, bool variable_arity = false) {
    //ODD_ASSERT(stack_size <= 1);
    emit(OP_RETURN);
    ODD_CC_EMIT("return");
    Prototype* p = 0;

    Blob* codeblob = 0, *localfreevars = 0, *upvals = 0, *dbginfo = 0;
    ODD_FRAME(p, codeblob, localfreevars, upvals, dbginfo);
    p = state.allocate<Prototype>();
    // Copy some data from the compiler to Scheme blobs
    codeblob = state.make_blob<size_t>(code.size());
    memcpy(codeblob->data, &code[0], code.size() * sizeof(size_t));
    p->code = codeblob;

    dbginfo = state.make_blob<debug_t>(debuginfo.size());
    memcpy(dbginfo->data, &debuginfo[0], debuginfo.size() * sizeof(debug_t));
    p->debuginfo = dbginfo;
    // print debug info
    
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
    p->name = *name;
    // TODO: Compress vector
    p->constants = *constants;
    p->arguments = arguments;
    if(variable_arity)
      p->set_header_bit(PROTOTYPE, Value::PROTOTYPE_VARIABLE_ARITY_BIT);
    return p;
  }

  // Clear a Compiler instance for re-use
  // Gets rid of code, upvals, etc, while preserving module environment
  void clear() {
    code.clear();
    debuginfo.clear();
    free_variables.clear();
    local_free_variable_count = upvalue_count = stack_max = stack_size = locals = last_insn = 0;
    constants = (Vector*) ODD_FALSE;
    closure = false;
    name = (String*) ODD_FALSE;
    escaped_variables = (Vector*) ODD_FALSE;
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
  push_stack_trace((prototype), (ip)); \
  VM_RETURN(x); \
}

size_t vm_depth;
// Passing arguments for tail calls. No need to protect 
// for garbage collector because no allocations will be triggered while
// trampolining.
std::vector<Value*> vm_trampoline_arguments;
// A garbage-collector protected vector. May be cleared when calling other functions!
std::vector<Value*> safestack;
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
void print_stack_trace(std::ostream& os) {
  for(size_t i = stack_trace.size(); i--;) {
    os << stack_trace[i] << std::endl;
  }
}

void push_stack_trace(Prototype* p, unsigned ip) {
  // Debuginfo is a blob containing structs with 3 unsigned integers.

  // The compiler spits these out as it compiles expressions. The first integer describes the instruction pointer,
  // and the second describe file and line information.

  // So we go back and find the integer that is closest to (but not over) the CURRENT instruction pointer, and that
  // source code information is used to print a descriptive stack trace.

  SourceInfo info;
  bool found = false;
  for(size_t i = 0; i < p->debuginfo->blob_length<debug_t>(); i++) {
    debug_t debug = p->debuginfo->blob_ref<debug_t>(i);
    if(ip < debug.first) break;
    info = debug.second;
    found = true;
  }
  if(found) {
    std::ostringstream out;
    out << source_names[info.file] << ':' << info.line << ' ' << (p->name ? p->name->string_data() : "anonymous") << ' ';
    // Source code may not be available (eg REPL)
    if(source_contents[info.file]->size() >= info.line) {
      std::string ln(source_contents[info.file]->at(info.line-1));
      // Erase beginning spaces
      ln.erase(ln.begin(), std::find_if(ln.begin(), ln.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
      out << '"' << ln << '"';
    }
    stack_trace.push_back(out.str());
  }
}

void push_stack_trace(const std::string& str) {
  stack_trace.push_back(str);
}

Value* arity_error_least(unsigned expected, unsigned got) {
  std::ostringstream out;
  out << "function expected at least " << expected << " arguments but got " << got;
  return make_exception(State::S_ODD_EVAL, out.str());
}

Value* arity_error_most(unsigned expected, unsigned got) {
  std::ostringstream out;
  out << "function expected at most " << expected << " arguments but got " << got;
  return make_exception(State::S_ODD_EVAL, out.str());
}

Value* arity_error() {
  return make_exception(State::S_ODD_EVAL, "function got bad amount of arguments");
}

Value* expected_applicable(const std::string& name, size_t arg_number,
                           Type got) {
  std::stringstream out;
  out << name << " expected argument " << arg_number+1
      << " to be applicable, but got " << got;
  return make_exception(State::S_ODD_TYPE, out.str());
}

Value* expected_list(const std::string& name, size_t arg_number, Value* lst) {
  std::ostringstream out;
  out << name << " expected argument " << arg_number+1 << " to be a list";
  return make_exception(State::S_ODD_TYPE, out.str());
}

Value* type_error(const std::string& name, size_t arg_number,
                  Value* argument, Type expected, Type got) {
  std::ostringstream out;
  out << name << " expected argument " << arg_number+1 << " to be " << 
    expected << " but got " << got << " (" << argument << ')';
  return make_exception(State::S_ODD_TYPE, out.str());
}

// Apply Scheme function
Value* vm_apply(Prototype* prototype, Closure* closure, size_t argc, Value* args[], bool& trampoline) {
  // Track virtual machine depth
  vm_depth++;

  ODD_VM_MSG("entering function ");
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
    
    f.upvalues = (Upvalue**) alloca(prototype->local_free_variable_count
                                    * sizeof(Upvalue*));
    memset(f.upvalues, 0, prototype->local_free_variable_count
                          * sizeof(Upvalue*));

    for(size_t i = 0; i != prototype->local_free_variable_count; i++) {
      unsigned index = data[i];
      f.upvalues[i] = allocate<Upvalue>();
      f.upvalues[i]->local = &f.locals[index];
    }
  }

  // Load arguments

  // Check argument count
  if(argc < prototype->arguments) { VM_RETURN(arity_error_least(prototype->arguments, argc)); }
  else if(argc > prototype->arguments && !prototype->prototype_variable_arity()) {
    VM_RETURN(arity_error_most(prototype->arguments, argc));
  }

  // Load normal arguments
  size_t i;
  for(i = 0; i != prototype->arguments; i++) f.locals[i] = args[i];

  // If we're not done yet, either we've received too many arguments or this
  // is a variable arity function
  if(i != argc) {
    if(prototype->prototype_variable_arity()) {
      return make_exception(State::S_ODD_EVAL,
                            "variable arity not supported yet");
    } else {
      VM_RETURN(arity_error());
    }
  }

  ODD_VM_MSG("evaluating function with stack size " << prototype->stack_max << " and " << prototype->locals << " locals");

  // Some common things you will see:
  // f.stack[f.si-1] Get the top of the stack 
  // f.stack[--f.si] Pop the top of the stack
  // wc[ip++] Get the next instruction and advance the instruction pointer

  // Evaluate function
  unsigned ip = 0;
  while(1) {
    switch(wc[ip++]) {
      case OP_PUSH_IMMEDIATE:
        ODD_VM_VMSG(ip-1 << " push-immediate " << (Value*) wc[ip]);
        f.stack[f.si++] = (Value*) wc[ip++]; 
        continue;
      case OP_PUSH_CONSTANT:
        f.stack[f.si++] = f.p->constants->vector_ref(wc[ip++]);
        continue;
      case OP_POP:
        ODD_VM_VMSG(ip-1 << " pop");
        --f.si;
        continue;
      case OP_GLOBAL_SET: {
        // Get the global name
        Symbol* k = ODD_CAST(Symbol, f.stack[f.si-1]);
        --f.si;
        // Pop the value off the stack
        Value* v = f.stack[--f.si];
        ODD_VM_VMSG("global-set " << k << ' ' << v);
        // Set the value
        k->value = v;
        continue;
      }
      case OP_GLOBAL_GET: {
        // Pop the global name (this is guaranteed to be a symbol)
        Symbol* k = ODD_CAST(Symbol, f.stack[f.si-1]);
        f.si--;
        // TODO: error messages
        f.stack[f.si++] = k->value;
        continue;
      }
      case OP_LOCAL_SET: {
        // Get the local's index
        size_t index = wc[ip++];
        ODD_VM_VMSG(ip-2 << " local-set " << (unsigned) index << " " << f.stack[f.si-1]);
        Value* val = f.stack[--f.si];
        f.locals[index] = val;
        continue;
      }
      case OP_LOCAL_GET: {
        // Get the local index
        size_t index = wc[ip++];
        ODD_VM_VMSG("local-get " << (unsigned)index);
        f.stack[f.si++] = f.locals[index];
        continue;
      }
      case OP_UPVALUE_GET: {
        // Get upvalue index
        size_t index = wc[ip++];
        ODD_VM_VMSG("upvalue-get " << (unsigned)index);
        f.stack[f.si++] = closure->upvalues->data[index]->upvalue();
        continue;
      }
      case OP_UPVALUE_SET: {
        // Get upvalue index
        size_t index = wc[ip++];
        Value* val = f.stack[--f.si];
        ODD_VM_VMSG("upvalue-set " << (unsigned)index);
        closure->upvalues->data[index]->upvalue_set(val);
        continue;
      }
      case OP_CLOSE_OVER: {
        // Create a closure
        ODD_VM_VMSG("close-over");
        Closure* c = 0;
        Prototype* p = ODD_CAST(Prototype, f.stack[f.si-1]);
        Blob* locations = p->upvalues;
        Vector* upvalues = 0;
        ODD_S_FRAME(c, p, locations, upvalues);
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
        ODD_VM_VMSG("return");
        // Get return value
        Value* ret = f.si ? f.stack[--f.si] : ODD_UNSPECIFIED;
        VM_RETURN(ret);
      }
      case OP_TAIL_APPLY: {
        // TODO: Debug-only?
        if(optimize_tail_calls) {
          // Retrieve argument count
          size_t argc = wc[ip++];
          ODD_VM_VMSG("tail-apply " << argc);
          // Pop the function, which should be at the top of the stack
          Value* fn = f.stack[f.si-1];
          f.si--;
          vm_trampoline_arguments.clear();
          for(size_t i = 0; i != argc; i++) {
            vm_trampoline_arguments.push_back(f.stack[f.si-argc+i]);
          }
          vm_trampoline_function = fn;
          trampoline = true;
          VM_RETURN(ODD_UNSPECIFIED);
        }
        // Note: Falls through to OP_APPLY if tail calls are disabled
      }
      case OP_APPLY: {
        // Retrieve argument count
        size_t argc = wc[ip++];
        ODD_VM_VMSG("apply " << (unsigned)argc);
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
        ODD_VM_VMSG("apply result: " << tmp);
        continue;
      }
      case OP_JUMP_IF_FALSE: {
        size_t insn = wc[ip++];
        ODD_VM_VMSG("jump-if-false "
                    << (f.stack[f.si-1] == ODD_FALSE ? "jmping" :
                        "not jmping") 
                    << " " << insn);
        Value* condition = f.stack[--f.si];
        if(condition == ODD_FALSE) {
          ip = insn;
        } 
        continue;
      }
      case OP_JUMP: {
        ODD_VM_VMSG("jump " << wc[ip]);
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
      } else {
        return result;
      }
    case NATIVE_FUNCTION: {
      NativeFunction* fn = static_cast<NativeFunction*>(function);
      NativeFunction::ptr_t ptr = fn->pointer;

      if(argc < fn->arguments) return arity_error_least(argc, fn->arguments);
      else if(argc > fn->arguments && !fn->native_function_variable_arity()) {
        return arity_error_most(argc, fn->arguments);
      }

      return ptr(*this, argc, args, fn->closure);
    }
    default:
      if(frames_lost) {
        std::ostringstream lost;
        lost << "Lost " << frames_lost << " stack frames to tail call optimization.";
        push_stack_trace(lost.str());
      }
      std::ostringstream desc;
      desc << "attempt to apply non-function " << prototype;
      return make_exception(State::S_ODD_EVAL, desc.str());
  }
  return ODD_UNSPECIFIED;
} 

// Apply with no arguments
Value* apply(Value* fn) { return apply(fn, 0, NULL); }

Value* apply(Value* fn, Value* arg1) {
  safestack.clear();
  safestack.push_back(arg1);
  return apply(fn, 1, &safestack[0]);
}

#undef VM_POP_FRAME
#undef VM_RETURN
#undef VM_CHECK

// module management
Handle<Table> modules;
std::vector<std::string> module_search_paths;
// compiler used for one-off evals, etc
Compiler* toplevel_cc;
std::vector<std::string> stack_trace;

// Convert a Scheme module name into a string
// [scheme base] => "#scheme.base"
// scheme => "#scheme"
Value* convert_module_name(Value* spec) {
  std::ostringstream out;
  out << '#';
  if(spec->get_type() == SYMBOL) {
    out << spec;
    return make_string(out.str());
  }
  if(!listp(spec)) 
    return make_exception(S_ODD_EVAL, "module name must be a list of symbols");
  while(spec->get_type() == PAIR) {
    if(unbox(spec->car())->get_type() != SYMBOL) {
      return make_exception(S_ODD_EVAL, "encountered non-symbol in module name");
    }
    out << unbox(spec->car());
    if(spec->cdr() != ODD_NULL) {
      out << '#';
    }
    spec = spec->cdr();
  }
  return make_string(out.str());
}

// Write module name like it'd be written in an import statement
// eg #scheme#base becomes scheme.base
static std::string write_module_name(String* name) {
  std::string str;
  if(!name->string_length()) return str;
  // skip # at beginning
  const char* data = name->string_data();
  for(size_t i = 1; i <= name->string_length(); i++) {
    if(data[i] == '#') str += '.';
    else str += data[i];    
  }
  return str;
}

bool module_loaded(String* name) {
  return table_contains(*modules, name);
}

Value* register_module(const char* name, Value* env) {
  String* sname = 0;
  ODD_S_FRAME(sname);
  sname = make_string(name);
  return register_module(sname, env);
}

Value* register_module(String* name, Value* env) {
  if(module_loaded(name)) {
    std::ostringstream os;
    os << "attempt to overload existing module " << name;
    return make_exception(S_ODD_EVAL, os.str());
  }
  table_insert(*modules, name, env);
  return ODD_FALSE;
}

Value* load_module(String* name) {
  if(module_loaded(name)) {
    return table_get(*modules, name);
  }

  // Convert internal module to OS path
  // eg #test#module becomes test/module.odd
  std::string path;
  const char* string = name->string_data();
  for(size_t i = 1; i < name->string_length(); i++) {
    char c = string[i];
    if(c == '#') {
      path += '/';
      continue;
    }
    path += c;
  }
  path += ".odd";

  if(!module_search_paths.size()) 
    ODD_FAIL("no module search paths added (try state.module_search_paths.push_back(\"./\"); before attempting to load modules");

  for(size_t i = module_search_paths.size(); i != 0; i--) {
    std::string whole_path(module_search_paths[i-1]);
    whole_path += path;

    std::ifstream fs(whole_path.c_str());
    // If file doesn't exist, keep searching
    if(!fs) continue;    

    // Load the module
    Value* chk = load_file(whole_path.c_str(), name);
    // Failure to load
    if(chk->active_exception()) return chk;
    // Now check that the file actually provided this module

    if(module_loaded(name)) {
      return table_get(*modules, name);
    }
  }

  std::ostringstream ss;
  ss << "failed to find file " << path;
  ss << " searched paths [";
  for(size_t i = 0; i != module_search_paths.size(); i++) {
    ss << module_search_paths[i] << (i != (module_search_paths.size() - 1) ? " " : "");
  }
  ss << ']';

  return make_exception(State::S_ODD_COMPILE, ss.str());
}

Value* load_module(const std::string& name) { return load_module(make_string(name)); }

// Evaluation functions
Value* load_file(const char* path, Value* module_name = 0) {
  unsigned fid = register_file(path);
  if(fid == 0) return ODD_FALSE;

  std::ifstream handle(path);
  ODD_ASSERT(handle);

  State::Reader reader(*this, handle, fid);

  Value* x = 0;
  Value* chk = 0;
  Prototype* p = 0;
  ODD_S_FRAME(x, p, chk, module_name);
  if(module_name)
    toplevel_cc->enter_module(module_name);
  else
    toplevel_cc->enter_module(table_get(*user_module, global_symbol(S_MODULE_NAME)));
  while(true) {
    x = reader.read();
    if(x == ODD_EOF) break;
    if(x->active_exception()) return x;
    chk = toplevel_cc->compile(x);
    if(chk != ODD_FALSE) return chk;
    p = toplevel_cc->end();
    ODD_CHECK(p);
    x = apply(p, 0, 0);
    ODD_CHECK(x);
    toplevel_cc->clear();
  }
  return x;
}

Value* eval(Value* exp, Value* env) {
  Table* new_env = 0;
  Value *chk = 0, *proto = 0, *result = 0;
  ODD_S_FRAME(exp, env, new_env, chk, proto, result);
  new_env = make_env(env);

  Compiler cc(*this, NULL, new_env);
  chk = cc.compile(exp, true);
  ODD_CHECK(chk);
  proto = cc.end();
  return apply(proto, 0, 0);
}

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
    ODD_ASSERT(!next);
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
  ODD_ASSERT("odd::Frame FIFO order violated" && state.frame_list == this);
  state.frame_list = previous;
}

///// (UTIL) Utilities

// Output

inline std::ostream& print_table(std::ostream& os, Table* t) {
  ODD_ASSERT(t->get_type() == TABLE);
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

// TYPENOTE
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
    case SYNCLO: os << "a syntactic closure"; break;
    case TABLE: os << "a table"; break;
    case FIXNUM: os << "a fixnum"; break;
    case CONSTANT: os << "a constant"; break;
  }
  return os;
}

inline std::ostream& operator<<(std::ostream& os, Value* x) {
  switch(x->get_type()) {
    case CONSTANT:
      if(x == ODD_TRUE) return os << "#t";
      if(x == ODD_FALSE) return os << "#f";
      if(x == ODD_EOF) return os << "#<eof>";
      if(x == ODD_NULL) return os << "()";
      if(x == ODD_UNSPECIFIED) return os << "#<unspecified>";
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
      os << '[' << x->car();
      for(x = x->cdr(); x->get_type() == PAIR; x = x->cdr()) {
        os << ' ' << x->car();
      }
      // Dotted list
      if(x != ODD_NULL) {
        os << " . " << x;
      }
      return os << ']';
    }  
    case BOX: {
      // If it has source info then it's some value wrapped by the reader
      if(x->box_has_source())
        return os << '$' << x->box_value();
      else
        return os << "#<box " << (ptrdiff_t)x << ">";
    }
    case EXCEPTION: {
      return os << "#<exception " << x->exception_tag() << ' ' 
                << x->exception_message() << '>';
    }
    case PROTOTYPE:
      return os << "#<function name: " << x->prototype_name() << '>';
    case CLOSURE:
      return os << "#<function name: "
                << static_cast<Closure*>(x)->prototype->prototype_name()
                << " closure>";
    case NATIVE_FUNCTION:
      return os << "#<function"
                << (static_cast<NativeFunction*>(x)->closure
                    ? " closure" : "")
                << " native>";
    case UPVALUE: return os << "#<upvalue "
                            << (x->upvalue_closed() ? "(closed) " : "")
                            << " " << x->upvalue() << ')';
    case SYNCLO: {
      os << "#<synclo " << x->synclo_expr();
      if(x->synclo_escaped_variables())
        os << ' ' << x->synclo_escaped_variables();
      return os << '>';
    }
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


///// (STD) Standard Scheme functions implemented in C++


#define ODD_CHECK_TYPE(expect, number) \
  if(args[(number)]->get_type() != (expect)) { \
    return state.type_error(fn_name, (number), args[(number)], \
                            (expect), args[(number)]->get_type()); \
  }

#define ODD_CHECK_APPLICABLE(number) \
  if(!((args[(number)])->applicablep())) { \
    return state.expected_applicable(fn_name, (number), \
                                     args[(number)]->get_type()); \
  }

// Check for maximum arguments; not necessary for fixed-arity functions but useful for functions which accept optional,
// but not unlimited arguments
#define ODD_MAX_ARITY(expect) \
  if((argc) > (expect)) { \
    std::ostringstream ss; \
    ss << fn_name << " expected no more than " << (expect) << " arguments, but got " << (argc); \
    return state.make_exception(State::S_ODD_EVAL, ss.str()); \
  }
    

ODD_FUNCTION(eq) {
  return Value::to_boolean(args[0] == args[1]);
}

ODD_FUNCTION(fx_gt) {
  static const char* fn_name = "fx>";
  ODD_CHECK_TYPE(FIXNUM, 0);
  ODD_CHECK_TYPE(FIXNUM, 1);
  return Value::to_boolean(args[0] > args[1]);
}

ODD_FUNCTION(fx_lt) {
  static const char* fn_name = "fx<";
  ODD_CHECK_TYPE(FIXNUM, 0);
  ODD_CHECK_TYPE(FIXNUM, 1);
  return Value::to_boolean(args[0] < args[1]);
}

#define ODD_PREDICATE(name, tipe) ODD_FUNCTION(name) { return Value::to_boolean(args[0]->get_type() == (tipe)); }

ODD_PREDICATE(pairp, PAIR);
ODD_PREDICATE(symbolp, SYMBOL);
ODD_PREDICATE(stringp, STRING);

#undef ODD_PREDICATE

ODD_FUNCTION(car) {
  static const char* fn_name = "car";
  ODD_CHECK_TYPE(PAIR, 0);
  return args[0]->car();
}

ODD_FUNCTION(cdr) {
  static const char* fn_name = "car";
  ODD_CHECK_TYPE(PAIR, 0);
  return args[0]->cdr();
}

ODD_FUNCTION(list) {
  //static const char* fn_name = "list";
  if(argc == 0) return ODD_NULL;
  // GC Protect arguments
  state.safestack.clear();
  for(size_t i = 0; i != argc; i++)
    state.safestack.push_back(args[i]);
  Value* head = ODD_NULL;
  ODD_FRAME(head);
  ptrdiff_t i = argc;
  while(i--) {
    head = state.cons(state.safestack[i], head);
  }
  return head;
}

ODD_FUNCTION(list_ref) {
  static const char* fn_name = "list-ref";
  if(args[0] == ODD_NULL) return ODD_NULL;
  ODD_CHECK_TYPE(PAIR, 0);
  ODD_CHECK_TYPE(FIXNUM, 1);
  ptrdiff_t i = args[1]->fixnum_value();
  Value* head = args[0];
  while(i-- && head->get_type() == PAIR) {
    head = head->cdr();
  }
  return head ? head->car() : ODD_FALSE;
}

ODD_FUNCTION(length_) {
  const char* fn_name = "length";
  if(args[0] == ODD_NULL) return Value::make_fixnum(0);
  ODD_CHECK_TYPE(PAIR, 0);
  Value* lst = args[0];
  ptrdiff_t num = 0;
  while(lst->get_type() == PAIR) {
    num++;
    if(lst->cdr() == ODD_NULL) break;
    if(lst->cdr()->get_type() != PAIR) return state.expected_list("length", 0, lst);
    lst = lst->cdr();
  }
  return Value::make_fixnum(num);
}

ODD_FUNCTION(apply_macro) { 
  // static const char* fn_name = "apply-macro";
  ODD_ASSERT(argc == 2);
  ODD_ASSERT(closure);
  ODD_ASSERT(closure->length == 2);
  Value *cc_env = closure->data[0], *transformer = closure->data[1], *exp = args[0], *result = 0;
  Value* other_env = args[1];

  ODD_FRAME(cc_env, transformer, result, exp, other_env);

  // Apply transformer
  state.safestack.clear();
  state.safestack.push_back(exp);
  state.safestack.push_back(other_env);

  result = state.apply(transformer, 2, &state.safestack[0]);
  return result;
}

ODD_FUNCTION(synclo) {
  const char* fn_name = "synclo";
  ODD_MAX_ARITY(3);
  ODD_CHECK_TYPE(TABLE, 0);
  Synclo* c = 0;
  Value* env = ODD_CAST(Table, args[0]);
  Value* expr = args[1];
  Vector* v = 0;
  Value* vars = (argc > 2) ? args[2] : ODD_FALSE;
  ODD_FRAME(c, env, expr, v, vars);
  c = state.allocate<Synclo>();
  c->env = ODD_CAST(Table, args[0]);
  c->expr = state.unbox(args[1]);
  if(argc > 2) {
    ODD_CHECK_TYPE(PAIR, 2);
    if(!state.listp(args[2])) {
      return state.expected_list("synclo", 2, args[2]);
    }
    v = state.make_vector();
    while(vars->get_type() == PAIR) {
      state.vector_append(v, state.unbox(vars->car()));
      vars = vars->cdr();
    }
    c->escaped_variables = v;
  }

  return c;
}

ODD_FUNCTION(string_to_symbol) {
  const char* fn_name = "string->symbol";
  ODD_CHECK_TYPE(STRING, 0);
  return state.make_symbol(ODD_CAST(String, args[0]));
}

ODD_FUNCTION(throw_) {
  const char* fn_name = "throw";
  ODD_CHECK_TYPE(SYMBOL, 0);
  ODD_CHECK_TYPE(STRING, 1);
  ODD_MAX_ARITY(3);

  Value* tag = args[0], *message = args[1], *irritants = args[2];
  Value* exc = 0;

  ODD_FRAME(tag, message, irritants, exc);
  exc = state.make_exception(ODD_CAST(Symbol, tag), ODD_CAST(String, message), irritants);
  return exc;
}

ODD_FUNCTION(catch_) {
  const char* fn_name = "catch";
  if(args[0] != 0) {
    ODD_CHECK_TYPE(SYMBOL, 0);
  }
  ODD_CHECK_APPLICABLE(1);
  ODD_CHECK_APPLICABLE(2);
  Symbol* tag = static_cast<Symbol*>(args[0]);
  Value* try_thunk = args[1], *except_thunk = args[2];
  Value* result = 0, *result2 = 0;
  ODD_FRAME(tag, try_thunk, except_thunk, result, result2);

  result = state.apply(try_thunk);
  if(!result->active_exception()) return result;
  // Check whether this particular exception should be handled or not
  if(!(tag == ODD_FALSE || tag == result->exception_tag())) return result;
  // Now deactivate the exception for handling
  result->unset_header_bit(EXCEPTION, Value::EXCEPTION_ACTIVE_BIT);
  
  result2 = state.apply(except_thunk, result);
  
  return result2;  
}

ODD_FUNCTION(syntax_error) {
  const char* fn_name = "syntax-error";
  ODD_CHECK_TYPE(STRING, 1);
  Value* irritant = args[0];
  String* message = ODD_CAST(String, args[1]);

  // Attempt to determine source of expression.
  std::string out;
  state.format_source_error(irritant, message->string_data(), out);
  return state.make_exception(State::S_ODD_SYNTAX, out);
}

ODD_FUNCTION(syntax_assert) {
  const char* fn_name = "syntax-assert";
  ODD_CHECK_TYPE(STRING, 1);
  Value* irritant = args[0];
  String* message = ODD_CAST(String, args[1]);
  Value* test = args[2];

  if(test != ODD_FALSE)
    return ODD_UNSPECIFIED;

  std::string out;
  state.format_source_error(irritant, message->string_data(), out);
  return state.make_exception(State::S_ODD_SYNTAX, out);
}

ODD_FUNCTION(print) {
  for(size_t i = 0; i != argc; i++) {
    if(args[i]->get_type() == STRING) {
      std::cout << args[i]->string_data() << (i == argc-1 ? "" : " ");
      continue;
    }
    std::cout << args[i] << (i == argc-1 ? "" : " ");
  }
  std::cout << std::endl;
  return ODD_UNSPECIFIED;
}

inline void State::initialize_builtin_functions() {
  // comparison & predicates
  defun("=", &eq, 2, false);
  defun("fx<", &fx_lt, 2, false);
  defun("fx>", &fx_gt, 2, false);

  defun("pair?", &pairp, 1, false);
  defun("string?", &stringp, 1, false);
  defun("symbol?", &symbolp, 1, false);

  // pairs & lists
  defun("car", &car, 1, false);
  defun("cdr", &cdr, 1, false);
  defun("list", &list, 0, true);
  defun("list-ref", &list_ref, 2, false);
  defun("length", &length_, 1, false);

  // macros
  defun("synclo", &synclo, 2, true);
  defun("syntax-assert", &syntax_assert, 3, false);
  defun("syntax-error", &syntax_error, 2, false);

  // symbols
  defun("string->symbol", &string_to_symbol, 1, false);
  
  // exceptions & errors
  defun("throw", &throw_, 2, true);
  defun("catch", &catch_, 3, false);


  // input/output
  defun("print", &print, 1, true);
}

} // odd

using odd::operator<<;

#endif
