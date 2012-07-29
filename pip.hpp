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
// 4. (READ) Expression reader and source code tracking
// 5. (CC) Compiler
// 6. (VM) Virtual Machine
// 7. (UTIL) Utilities

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
#include <list>
#include <vector>
#ifdef __APPLE__
# include <boost/unordered_map.hpp>
#else
# include <unordered_map> // TODO: Alternatives to c++0x hash tables
#endif

// Assertions. Since these can be quite expensive, they're only enabled when
// PIP_DEBUG is explicitly defined.
#ifdef PIP_DEBUG
# define PIP_ASSERT(x) (assert(x))
#else
# define PIP_ASSERT(x) ((void)0)
#endif

// Checking for exceptions
#define PIP_CHECK(x) if((x)->active_exception()) return (x);

// Debug mesages
#define PIP_GC_MSG(x) std::cout << x << std::endl
#define PIP_CC_MSG(x) \
    for(size_t _x = (depth); _x; _x--) \
      std::cout << '>'; std::cout << "CC: " << x << std::endl;
#define PIP_CC_VMSG(x) PIP_CC_MSG(x)
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

#ifdef __APPLE__
using boost::unordered_map;
#else
using std::unordered_map;
#endif

struct State;
struct Value;
struct String;
struct Symbol;
struct VectorStorage;

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

// Cast with assertions (will cause double evaluation; do not give an argument with side effects)
#define PIP_CAST(klass, x) (PIP_ASSERT((x)->get_type() == pip::klass ::CLASS_TYPE), (klass *) (x))

// Constant values 
#define PIP_FALSE ((pip::Value*) 0)
#define PIP_TRUE ((pip::Value*) 2)
#define PIP_NULL ((pip::Value*) 6)
#define PIP_EOF ((pip::Value*) 10)
#define PIP_UNSPECIFIED ((pip::Value*) 18)

// C++ object types (which may not match up exactly with Scheme predicates)
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
  // Immediate values
  FIXNUM,
  CONSTANT,
};

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

  // Methods for interacting with the header safely
  int get_header_bit(Type type, int bit) const { PIP_ASSERT(pointerp()); PIP_ASSERT(get_type() == type); return header & bit; }
  void set_header_bit(Type type, int bit) { PIP_ASSERT(get_type() == type); header += bit; }

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
    return fixnump() ? FIXNUM : (constantp() ? CONSTANT : static_cast<Type>(get_type_unsafe()));
  }

  // Type specific methods: simple getters and setters, object flags

  // Fixnums

  static Value* make_fixnum(ptrdiff_t n) { return (Value*) ((n << 1) + 1); }
  ptrdiff_t fixnum_value() const { return bits() >> 1; }

  // Pairs

  // True when the Pair has source code information attached
  static const int PAIR_HAS_SOURCE_BIT = 1 << 15;

  bool pair_has_source() const { return get_header_bit(PAIR, PAIR_HAS_SOURCE_BIT); }

  Value* car() const;
  Value* cdr() const;
  Value* cadr() const;
  Value* cddr() const;

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
  template <class T> T blob_set(unsigned, T& value) const;

  // Strings
  const char* string_data() const;
  unsigned string_length() const;

  // Symbols
  Value* symbol_name() const;
  Value* symbol_value() const;

  // Exceptions

  // True when an exception is active and should be passed on
  static const int EXCEPTION_ACTIVE_BIT = 1 << 15;
  
  bool active_exception() const { return get_type() == EXCEPTION && (header & EXCEPTION_ACTIVE_BIT); }

  Symbol* exception_tag() const;
  String* exception_message() const;
  Value* exception_irritants() const;

  // Boxes

  // True when a box contains source code information
  static const int BOX_HAS_SOURCE_BIT = 1 << 15;

  bool box_has_source() const { return get_header_bit(BOX, BOX_HAS_SOURCE_BIT); }

  Value* box_value() const;

  // Prototypes
  static const int PROTOTYPE_VARIABLE_ARITY_BIT = 1 << 15;

  bool prototype_variable_arity() const { return get_header_bit(PROTOTYPE, PROTOTYPE_VARIABLE_ARITY_BIT); }

  // Upvalues
  static const int UPVALUE_CLOSED_BIT = 1 << 15;

  bool upvalue_closed() const { return get_header_bit(UPVALUE, UPVALUE_CLOSED_BIT); }

  Value* upvalue() const;
  void upvalue_set(Value*);
  void upvalue_close();
};

struct Pair : Value {
  Value *car, *cdr;
  // Source code information; only available when pair_has_source() is true
  SourceInfo src;
  
  static const Type CLASS_TYPE = PAIR;
};

Value* Value::car() const { PIP_ASSERT(get_type() == PAIR); return static_cast<const Pair*>(this)->car; }
Value* Value::cdr() const { PIP_ASSERT(get_type() == PAIR); return static_cast<const Pair*>(this)->cdr; }
Value* Value::cadr() const { PIP_ASSERT(get_type() == PAIR); return static_cast<const Pair*>(this)->cdr->car(); }
Value* Value::cddr() const { PIP_ASSERT(get_type() == PAIR); return static_cast<const Pair*>(this)->cdr->cdr(); }

void Value::set_car(Value* car_)  { PIP_ASSERT(get_type() == PAIR); static_cast<Pair*>(this)->car = car_; }
void Value::set_cdr(Value* cdr_)  { PIP_ASSERT(get_type() == PAIR); static_cast<Pair*>(this)->cdr = cdr_; }

struct VectorStorage : Value {
  unsigned length;
  Value* data[1];

  static const Type CLASS_TYPE = VECTOR_STORAGE;
};

Value** Value::vector_storage_data() const { PIP_ASSERT(get_type() == VECTOR_STORAGE); return (Value**)static_cast<const VectorStorage*>(this)->data; }

struct Vector : Value {
  VectorStorage* storage;
  unsigned capacity;

  static const Type CLASS_TYPE = VECTOR;
};

VectorStorage* Value::vector_storage() const { PIP_ASSERT(get_type() == VECTOR); return static_cast<const Vector*>(this)->storage; }
Value** Value::vector_data() { return vector_storage()->data; }
Value* Value::vector_ref(size_t i) { return vector_storage()->data[i]; }
void Value::vector_set(size_t i, Value* v) { vector_storage()->data[i] = v; }
size_t Value::vector_length() const { return vector_storage()->length; }

struct Blob : Value {
  unsigned length;
  char data[1];

  static const Type CLASS_TYPE = BLOB;
};

const char* Value::blob_data() const { PIP_ASSERT(get_type() == BLOB); return static_cast<const Blob*>(this)->data; }
unsigned Value::blob_length() const { PIP_ASSERT(get_type() == BLOB); return static_cast<const Blob*>(this)->length; }
template <class T> unsigned Value::blob_length() const { PIP_ASSERT(get_type() == BLOB); return static_cast<const Blob*>(this)->length / sizeof(T); }
template <class T> T Value::blob_ref(unsigned i) const { PIP_ASSERT(get_type() == BLOB); return ((T*) static_cast<const Blob*>(this)->data)[i]; }
template <class T> T Value::blob_set(unsigned i, T& val) const { PIP_ASSERT(get_type() == BLOB); ((T*) static_cast<const Blob*>(this)->data)[i] = val; }

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

// Prototype for a procedure containing constants, code, and debugging
// information. May be executed directly if the function does not reference
// free variables.
struct Prototype : Value {
  Vector* constants;
  Blob* code;
  Blob* debuginfo;
  // A list of local free variables referenced by other functions, which will
  // be converted into upvalues when the function ends
  Blob* local_free_variables;
  // A list of upvalues, which will be used to turn this into a closure if necessary
  Blob* upvalues;
  // The function's name, if available, or #f if not
  String* name;
  unsigned stack_max;
  unsigned locals;
  unsigned local_free_variable_count;
  unsigned arguments;

  static const Type CLASS_TYPE = PROTOTYPE;
};

// A closure, aka a procedure which references free variables
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

///// (GC) Garbage collector

// <--
// Pip's garbage collector is a mark-and-don't-sweep collector. The allocator
// is a first-fit allocator: it allocates memory from the first appropriately
// sized free chunk of memory it can find. It marks every chunk of memory it
// sweeps over (or, if the chunk is already marked, it skips it). When the
// allocator reaches the end of the heap, the meaning of the mark bit is
// flipped, and the entire heap is considered garbage. Then the collector
// immediately marks all live memory, and we know which objects are marked. The
// benefit of a mark-and-don't-sweep collector is that the work is spread out
// amongst allocations and should (in most cases) result in no large pauses to
// the program. It requires no external data structures; the only overhead
// being that each object needs a "size" and "mark" field (and in Pip's case,
// the mark field is stored alongside type and other flags in a space efficient
// manner). The main downside is fragmentation.

// How does Pip mark live memory? It starts with the program's "roots" and
// marks all their children and so on. But how do we find the roots? They are
// explicitly registered with the garbage collector using two methods: Frames
// and Handles. A Frame is a stack-allocated structure that keeps track of as
// many variables as needed. You create a frame in each function you want to
// track variables in like so: PIP_FRAME(state, var1, var2). A Handle is a
// heap-allocated structure that keeps track of one variable, and is useful
// when your variables have lifetimes beyond the execution of a single
// function.

// How does Pip get memory from the operating system? It starts by using a
// small amount of memory (4 kilobytes). If, after a collection, more than a
// certain amount of memory is in use (the "load factor", by default 77%), it
// allocates twice as much memory. It can also allocate more memory if a very
// large object (over 4kb) is allocated.
// -->

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
  PIP_(VectorStorage)
  PIP_(String)
  PIP_(Exception)
  PIP_(Box)
  PIP_(Prototype)
  PIP_(Closure)
  PIP_(Upvalue)
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

struct State {
  struct VMFrame;
  template <class T> struct Handle;

  State(): 

    // Garbage collector
    handle_list(0),
    frame_list(0), 
    collect_before_every_allocation(false), heap_size(PIP_HEAP_ALIGNMENT),
    mark_bit(1), block_cursor(0), sweep_cursor(0), live_at_last_collection(0),
    collections(0),

    // Reader
    source_counter(1),
   
    // Compiler
    global_env(*this),
    
    // Virtual machine
    vm_depth(0) {

    // Initialize garbage collector
    Block* first = new Block(PIP_HEAP_ALIGNMENT, false, mark_bit);
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
      // Special forms
      "define",
      "set!",
      "lambda",
      "named-lambda",
      "if",
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

    global_env = make_vector();

    vector_append(*global_env, PIP_FALSE);

    for(size_t i = S_DEFINE; i != S_QUOTE+1; i++) {
      vector_append(*global_env, global_symbol(static_cast<Global>(i)));
      vector_append(*global_env, global_symbol(S_SPECIAL));
    }

    // Initialize reader
    source_names.push_back("unknown");
    source_contents.push_back(NULL);
  }

  ~State() {
    for(size_t i = 0; i != globals.size(); i++)
      delete globals[i]; 

    for(size_t i = 0; i != blocks.size(); i++)
      delete blocks[i];

    for(size_t i = 0; i != source_contents.size(); i++)
      delete source_contents[i];
  }

  ///// RUNTIME
  typedef unordered_map<std::string, Symbol*> symbol_table_t;

  symbol_table_t symbol_table;
  std::vector<Handle<Value> *> globals;

  enum Global {
    S_SPECIAL,
    S_VARIABLE,
    S_UPVALUE,
    S_PIP_READ,
    S_PIP_COMPILE,
    S_PIP_EVAL,
    S_DEFINE,
    S_SET,
    S_LAMBDA,
    S_NAMED_LAMBDA,
    S_IF,
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

  template <class T>
  Blob* make_blob(ptrdiff_t length) {
    return make_blob(sizeof(T) * length);
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

  Vector* make_vector(unsigned capacity = 2) {
    capacity = capacity > 2 ? capacity : 2;
    Vector* v = 0;
    VectorStorage* s = 0;
    PIP_S_FRAME(v, s);
    v = allocate<Vector>();
    v->capacity = capacity;
    s = allocate<VectorStorage>((capacity * sizeof(Value*)) - sizeof(Value*));
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

  ///// GARBAGE COLLECTOR

  // Handles are auto-tracked heap-allocated pointers
  template <class T>
  struct Handle {
    Handle(State& state_): state(state_), ref(0) {
      initialize();
    }

    Handle(State& state_, T* ref_): state(state_), ref(ref_) {
      initialize();
    }

    void initialize() {
      previous = state.handle_list;
      if(previous)
        previous->next = (Handle<Value> *) this;
      state.handle_list = (Handle<Value> *) this;
      next = 0;
    }

    ~Handle() {
      if(previous) previous->next = next;
      if(next) next->previous = previous;
      if(state.handle_list == (Handle<Value> *) this) {
        PIP_ASSERT(!next);
        state.handle_list = previous;
      }
    }

    State& state;
    T* ref;
    Handle<Value> *previous, *next;
    std::list<Handle<Value>*>::iterator location;
  
    T* operator*() const { return ref; }
    T* operator->() const { return ref; }
    void operator=(T* ref_) { ref = ref_; }  
  };

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
        // Note that in order to save a little redunancy, objects are marked
        // based on their structure (eg all objects with two pointers are
        // marked as though they were pairs)
        // Atomic
        case STRING: case BLOB:
          return;
        // One pointer
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

  struct HeapSweep {
    HeapSweep(State& state_): state(state_), block_cursor(0), cursor(0), block(state_.blocks[0]) {
    }
    
    State& state;
    size_t block_cursor;
    char* cursor;
    Block* block;

    bool done() { return block_cursor == state.blocks.size(); }

    void next(size_t& oblock, Value*& ocursor) {

    }
  };

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
    for(symbol_table_t::iterator i = symbol_table.begin(); i != symbol_table.end(); i++)
      recursive_mark(i->second);

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

  }

  void compact() {
    // Quick collection
    finish_mark();
    mark_bit = !mark_bit;
    mark_heap();

    // Create a new block to copy the heap to
    Block* heap = new Block(align(PIP_HEAP_ALIGNMENT, heap_size), false, mark_bit);

    // Loop over live memory, allocating from the new block. The new
    // allocation is called the "forwarding pointer" and the object's
    // 'size' field is used to store the forwarding pointer (the newly
    // allocated space stores the object's size)


    // Loop over the heap and copy all objects, updating pointers as
    // you go.
  }

  void reset_sweep() {
    sweep_cursor = blocks[0]->begin;
    block_cursor = 0;
  }

  void collect(size_t request = 0, bool force_request = false) {
    collections++;

    // If collection is called early, we have to sweep over everything
    // and make sure it's marked as used
    finish_mark();

    // Reset sweep cursor
    reset_sweep();

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

      PIP_GC_MSG("growing heap from " << FriendlySize(heap_size - new_block_size) << " to " << FriendlySize(heap_size)
                 << " because of " << (pressure >= PIP_LOAD_FACTOR ? "pressure" : "allocation failure"));
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
  
  template <class T> T* allocate(ptrdiff_t additional = 0) {
    return static_cast<T*>(allocate(static_cast<Type>(T::CLASS_TYPE), sizeof(T) + additional));
  }


  ///// BASIC FUNCTIONS

  unsigned vector_append(Vector* vec, Value* value) {
    vec->storage->data[vec->storage->length++] = value;
    if(vec->storage->length == vec->capacity) {
      VectorStorage* s = 0;
      PIP_S_FRAME(vec, s);
      size_t capacity = vec->capacity * 2;
      s = allocate<VectorStorage>((capacity * sizeof(Value*)) - sizeof(Value*));
      memcpy(s->data, vec->storage->data, (vec->storage->length * sizeof(Value*)));
      s->length = vec->storage->length;
      vec->capacity = capacity;
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

  void format_source_error(Value* exp, const std::string& msg, std::string& out) {
    SourceInfo info;
    unwrap_source_info(exp, info);
    if(info.file) {
      std::ostringstream ss;
      ss << source_names[info.file] << ':' << info.line << ": " << msg << std::endl << "  " << source_contents[info.file]->at(info.line-1);
      out = ss.str();
    } else {
      out = msg;
    }
  }

  static bool listp(Value* lst) {
    if(lst == PIP_NULL) return true;
    if(lst->get_type() != PAIR) return false;
    while(lst->get_type() == PAIR) {
      lst = lst->cdr();
      if(lst == PIP_NULL) return true;
    }
    return false;
  }

  static size_t length(Value* lst) {
    size_t i = 0;
    while(lst->get_type() == PAIR) {
      i++;
      lst = lst->cdr();
    }
    return i;
  } 

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
  // such as #f and 12345, don't cause compilation or runtime errors and therefore
  // don't need to be wrapped.
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
    size_t length;
    std::fstream fs(path, std::fstream::in);
    std::string line;
    if(!fs.is_open()) return 0;
    source_names.push_back(path);
    read_lines(fs);

    return source_counter++;
  }

  // Load the source code of a string
  // "name" is ideally something describing where the string came from
  unsigned register_string(const std::string& name, const std::string& string) {
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
        c == EOF || isspace(c) || c == '.' || c == '#' || c == '"' || quotep(c);
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

    Reader(State& state_, std::istream& input_, unsigned file_): state(state_), input(input_),
    file(file_), line(1), token_value(state), token_lookahead(TK_LOOKAHEAD) {
      
    }
    ~Reader() { }

    State& state;
    std::istream& input;
    // File and current line
    unsigned file, line;
    // Values read in by the tokenizer, or exceptions encountered while tokenizing
    Handle<Value> token_value;
    Token token_lookahead;
    std::string token_buffer;

    // Error handling

    Token lex_error(const std::string& msg) {
      std::stringstream ss;
      ss << state.source_names[file] << ':' << line << ": " << msg;
      token_value = state.make_exception(State::S_PIP_READ, ss.str());
      return TK_EXCEPTION;
    }

    Value* parse_error(const std::string& msg) {
      std::stringstream ss;
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

        // If it's not a number or a symbol, it will begin with a single character
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
      PIP_FRAME(state, initial, head, tail, elt, swap);
      // If we've been given an initial element (say quote in a quoted expression), attach it
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
        if(token != TK_RPAREN) return parse_error("more than one element at the end of a dotted list");
        discard_lookahead();
      }

      // Attach source info by re-creating the head of the list with source information attached
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

  ///// (CC) Compiler

  // <--
  // The Pip compiler is a simple, one-pass compiler. An instance of the
  // compiler is created for each procedure (for the purposes of the compiler,
  // something like a library source file is considered a procedure). There is
  // no separate parsing step; the compiler operates directly on s-expressions.
  // It is a little hairy in places because of this, but doing everything in
  // one pass makes it much shorter.

  // The compiler compiles a very small subset of Scheme. Most special forms,
  // such as let, are implemented with macros.

  // Perhaps the most complex machinery in the compiler is the handling of free
  // variables (for Pip's purposes, a "free variable" is defined as any
  // variable that might exist after its function returns; I'm not sure that's
  // the right definition but I'm not changing it now). When a procedure
  // references, either by setting or getting, any variable from a higher
  // procedure, the compiler causes that variable to be pulled into every
  // procedure from that procedure on down as an "upvalue", which is basically
  // a heap-allocated pointer. All of those procedures become Closures, a
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
    OP_GLOBAL_SET = 3,
    OP_GLOBAL_GET = 4,
    OP_LOCAL_SET = 5,
    OP_LOCAL_GET = 6,
    OP_UPVALUE_SET = 7,
    OP_UPVALUE_GET = 8,
    OP_CLOSE_OVER = 9,
    OP_RETURN = 10,
    OP_APPLY = 11,
    OP_TAIL_APPLY = 12
  };

  struct UpvalLoc {
    UpvalLoc(bool l, unsigned i): local(l), index(i) {}
    bool local;
    unsigned index;
  };

  // Environments

  // Environment format
  // #(<parent environment> <key> <value> ...)

  Vector* make_env(Value* parent = PIP_FALSE) {
    Vector* env = 0;
    PIP_S_FRAME(env, parent);
    env = make_vector();
    vector_append(env, parent);
  }

  // Set an environment variable
  unsigned env_def(Vector* env, Value* key, Value* value) {
    for(size_t i = 1; i != env->vector_length(); i += 2) {
      if(env->vector_ref(i) == key) {
        env->vector_set(i+1, value);
        return ((i - 1) / 2) - 1;
      }
    }
    PIP_S_FRAME(env, key, value);
    unsigned slot = vector_append(env, key);
    vector_append(env, value);
    return ((slot - 1) / 2) - 1;
  }

  // Look up an environment variable (the result is rather complex, and returned in
  // a structure)

  enum RefScope { REF_GLOBAL, REF_UP, REF_LOCAL };
  struct Lookup {
    Lookup(): scope(REF_LOCAL), index(0), level(0) {}

    RefScope scope;
    unsigned index;
    unsigned level;
  };

  Value* env_ref(Vector* env, Value* key, Lookup& lookup) {
    Value *start_env = env;
    // Search through environments
    while(env->get_type() == VECTOR) {
      // Search through an environment
      for(size_t i = 1; i != env->vector_length(); i += 2) {
        // We found a match
        if(env->vector_ref(i) == key) {
          // If this environment has no parent environment, it's global
          if(env->vector_ref(0) == PIP_FALSE) {
            lookup.scope = REF_GLOBAL;
          } else {
            // Otherwise, if we're still searching the original environment,
            // it's a local variable, and if not, it's an upvalue
            lookup.scope = env == start_env ? REF_LOCAL : REF_UP;
          }
          // Calculate index value from location (we have to compensate for the
          // fact that both keys and values are stored, and start at 1)
          lookup.index = (i - 1) / 2;
          return env->vector_ref(i+1);
        }
      }
      // Search parent environment, if it exists
      lookup.level++;
      env = static_cast<Vector*>(env->vector_ref(0));
    }
    // Lookup failed
    return PIP_FALSE;
  }

  Handle<Vector> global_env;

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

  // An instance of the compiler, created for each procedure
  struct Compiler {
    Compiler(State& state_, Compiler* parent_, Vector* env_ = NULL, size_t depth_ = 0): 
      state(state_), parent(parent_),
      local_free_variable_count(0), upvalue_count(0),
      stack_max(0), stack_size(0), locals(0), constants(state), closure(false),
      env(0), depth(depth_) {
      if(!env_) env = &state.global_env;
      else env = new Handle<Vector>(state, env_);
    }
    ~Compiler() {
      if(env != &state.global_env) delete env;    
    }

    State& state;
    // The parent procedure -- necessary for free variables
    Compiler* parent;
    // The bytecode
    std::vector<unsigned char> code;
    // A vector containing combinations of instruction offsets and source location info for debugging and tracebacks
    std::vector<std::pair<unsigned, SourceInfo> > debuginfo;
    // A vector containing free variable locations
    std::vector<FreeVariableInfo> free_variables;
    unsigned local_free_variable_count, upvalue_count;

    unsigned stack_max, stack_size, locals;
    Handle<Vector> constants;
    bool closure;
    // The environment of the function
    Handle<Vector>* env;
    // The depth of the function (for debug message purposes)
    size_t depth;

    // Code generation
    void emit(unsigned char byte) {
      code.push_back(byte);
    }

    // In order to push a constant onto the stack, we have to save it and store
    // it along with the function. This function lazily creates the vector of
    // constants and makes sure no constants are repeated.
    void push_constant(Value* constant) {
      push();
      Vector* cs = *constants;
      PIP_FRAME(state, cs, constant);
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
          emit(i);
          PIP_CC_VMSG("push-constant " << constant << " " << i << " (existing constant)");
          return;
        }
      }
      // Constant does not exist, add to vector and then push
      unsigned i = state.vector_append(cs, constant);
      emit(OP_PUSH_CONSTANT);
      emit(i);
      PIP_CC_VMSG("emit push-constant " << constant << " " << i);
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
    Value* error(Value* form, const std::string& msg) {
      std::string out;
      state.format_source_error(form, msg, out);
      return state.make_exception(State::S_PIP_COMPILE, out);
    }

    Value* syntax_error(Value* form, const std::string& str) {
      std::string out;
      state.format_source_error(form, str, out);
      return state.make_exception(State::S_PIP_COMPILE, out);
    }

    Value* arity_error(Global name, Value* form, size_t expected, unsigned got) {
      std::ostringstream ss;
      ss << "malformed " << state.global_symbol(name) << ": expected " << expected << " arguments, but got " << got;
      syntax_error(form, ss.str());
    }

    Value* syntax_error(Global name, Value* form, const std::string& str) {
      std::ostringstream ss;
      ss << "malformed " << state.global_symbol(name) << ": " << str;
      syntax_error(form, ss.str());
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
        if(l.lexical_level == lookup.level && l.lexical_index == lookup.index)
          return i;
      }
      // New free variable - register it with this and all preceding functions as
      // necessary. 
      l.lexical_level = lookup.level;
      l.lexical_index = lookup.index;
      PIP_CC_VMSG("register free variable <" << l.lexical_level << ", " << l.lexical_index << '>');
      Lookup lookup_copy(lookup);
      lookup_copy.level--;
      if(lookup.level != 0) {
        // Register with preceding function, and make this a closure if it isn't already
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

    // Generate a set! (used by both define and set!)
    // Assumes value is already on the stack
    void generate_set(Lookup& lookup, Value* name) {
      switch(lookup.scope) {
        case REF_GLOBAL:
          push_constant(name);
          emit(OP_GLOBAL_SET);
          PIP_CC_VMSG("emit global-set " << name);
          pop(2);
          break;
        case REF_LOCAL:
          emit(OP_LOCAL_SET);
          emit(lookup.index);
          PIP_CC_VMSG("emit local-set " << (unsigned) lookup.index);
          pop();
          break;
        case REF_UP: assert(0);
      }
    }

    // Special forms
    Value* compile_set(size_t argc, Value* exp) {
      Value* chk = 0;
      // set always has two arguments
      if(argc != 2) return arity_error(S_SET, exp, 2, argc);
      Value* name = unbox(exp->cadr());
      // Check that name is a symbol
      if(name->get_type() != SYMBOL)
        return syntax_error(S_SET, exp, "first argument to set! must be a symbol");

      // Compile body 
      chk = compile(exp->cddr()->car());
      PIP_CHECK(chk);

      // Look up variable
      Lookup lookup;
      Value* type = state.env_ref(**env, name, lookup);
      // Don't allow special forms
      if(type == state.global_symbol(S_SPECIAL))
        return syntax_error(exp, "special forms cannot be used as variables");
      // Lookup was successful
      else if(type == state.global_symbol(S_VARIABLE)) {
        generate_set(lookup, name);
      } else assert(0);
      return PIP_FALSE;
    }

    Value* compile_define(size_t argc, Value* exp) {
      Value* chk = 0;
      if(argc != 2) return arity_error(S_DEFINE, exp, 2, argc);
      Value* name = unbox(exp->cadr());
      // TODO: lambda shortcut 
      if(name->get_type() != SYMBOL)
        return syntax_error(S_DEFINE, exp, "first argument to define must be a symbol");
      unsigned slot = state.env_def(**env, name, state.global_symbol(S_VARIABLE));
      // Compile body for set!
      chk = compile(exp->cddr()->car());
      PIP_CHECK(chk);
      // Global variable
      Lookup lookup;
      lookup.scope = (*env)->vector_ref(0) == PIP_FALSE ? REF_GLOBAL : REF_LOCAL;
      lookup.index = slot;
      generate_set(lookup, name);
      return PIP_FALSE;
    }

    Value* compile_lambda(size_t form_argc, Value* exp) {
      // For checking subcompilation results
      Value* chk = 0;
      if(form_argc < 2) return arity_error(S_LAMBDA, exp, 2, form_argc);
      // Create new compiler, environment
      unsigned argc = 0;
      // Will be set to true if the function is variable arity
      // (i.e. takes unlimited arguments)
      bool variable_arity = false;
      Value *new_env = 0, *args = 0, *body = 0;
      Prototype* proto = 0;
      PIP_FRAME(state, exp, new_env, args, body, proto);
      // Create a new environment for the function
      new_env = state.make_vector();
      // Make the current environment its parent environment
      state.vector_append(PIP_CAST(Vector, new_env), **env);
      // Now parse the function's arguments and define them within the
      // new environment
      args = exp->cadr();
      if(args != PIP_NULL) {
        if(args->get_type() != PAIR) 
          return syntax_error(S_LAMBDA, exp, "first argument to lambda must be either () or a list of arguments");
        while(args->get_type() == PAIR) {
          Value* arg = unbox(args->car());
          if(arg->get_type() != SYMBOL) {
            return syntax_error(S_LAMBDA, exp, "lambda argument list must be symbols");
          }
          // Finally, we can add them to the environment!
          state.vector_append(PIP_CAST(Vector, new_env), arg);
          state.vector_append(PIP_CAST(Vector, new_env), state.global_symbol(S_VARIABLE));
          argc++;
          args = args->cdr();
          if(args->get_type() != PAIR) {
            variable_arity = true;
            break;
          }
        }
      }
      // Compile the procedure's body
      PIP_CC_MSG("compiling subprocedure");
      Compiler cc(state, this, PIP_CAST(Vector, new_env), depth+1);
      body = exp->cddr();
      while(body != PIP_NULL) {
        chk = cc.compile(body->car());
        PIP_CHECK(chk);
        body = body->cdr();
      }
      proto = cc.end(argc, variable_arity);
      push_constant(proto);
      // If the function's a closure, we'll have to close over it
      if(cc.closure) {
        PIP_CC_VMSG("emit close-over");
        emit(OP_CLOSE_OVER);
      }
      return PIP_FALSE;
    }

    // Compile a normal function application
    Value* compile_apply(Value* exp) {
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

      emit(OP_APPLY);
      PIP_ASSERT(argc < 255);
      emit((unsigned char) argc); 
      PIP_CC_VMSG("emit apply " << argc);
      // The application will pop the arguments and the function once finished
      pop(stack_size - old_stack_size);
      return PIP_FALSE;
    }

    // Compile a variable reference
    Value* compile_ref(Value* exp) {
      annotate(exp);
      Value* name = unbox(exp);
      Lookup lookup;
      Value* type = state.env_ref(**env, name, lookup);
      if(type == state.global_symbol(S_SPECIAL))
        return syntax_error(exp, "special forms cannot be used as values");
      if(type == state.global_symbol(S_VARIABLE)) {
        switch(lookup.scope) {
          case REF_GLOBAL:
            push_constant(name);
            emit(OP_GLOBAL_GET);
            PIP_CC_VMSG("emit global-get " << name);
            push();
            break;
          case REF_LOCAL:
            emit(OP_LOCAL_GET);
            emit((unsigned char)lookup.index);
            PIP_CC_VMSG("emit local-get " << name << ' ' << lookup.index);
            break;
          case REF_UP:
            unsigned index = register_free_variable(lookup);
            emit(OP_UPVALUE_GET);
            emit((unsigned char) index);
            PIP_CC_VMSG("emit upvalue-get " << name << ' ' << index);
            break;
        } 
      } else PIP_ASSERT(0);
      return PIP_FALSE;
    }

    // Compile a single expression, returns #f or an error if one occurred
    Value* compile(Value* exp) {
      // Dispatch based on an object's type
      switch(exp->get_type()) {
        // Constants: note that these are only immediate constants, such as #t
        // and #f, and not complex values that the compiler considers
        // constants, such as '(list)
        case CONSTANT:
          emit(OP_PUSH_IMMEDIATE);
          emit((unsigned char)(((size_t) exp) & 255));
          PIP_CC_VMSG("emit push-immediate " << exp);
          push();
          break;
        // Variable references
        case SYMBOL:
        case BOX: {
          return compile_ref(exp);
          break;
        }
        case PAIR: {
          // Compile applications, including special forms, macros, and normal function applications
          if(!listp(exp)) return error(exp, "dotted lists not allowed in source");
          // Note the source location of this application
          annotate(exp);
          // Check for special forms
          Lookup lookup;
          Value* type = state.env_ref(**env, unbox(exp->car()), lookup);
          // If this is a special form
          if(type == state.global_symbol(S_SPECIAL)) {
            // Dispatch on the special form
            Symbol* op = PIP_CAST(Symbol, unbox(exp->car()));
            size_t argc = length(exp->cdr());
            // define
            if(op == state.global_symbol(S_DEFINE)) {
              return compile_define(argc, exp);
            // set!
            } else if(op == state.global_symbol(S_SET)) {
              return compile_set(argc, exp);
            // lambda
            } else if(op == state.global_symbol(S_LAMBDA)) {
              return compile_lambda(argc, exp);
            }
          } else {
            // function application
            return compile_apply(exp);
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
      emit(OP_RETURN);
      PIP_CC_VMSG("emit return");
      Prototype* p = 0;

      Blob* codeblob = 0, *localfreevars = 0, *upvals = 0;
      PIP_FRAME(state, p, codeblob, localfreevars, upvals);
      p = state.allocate<Prototype>();
      // Copy some data from the compiler to Scheme blobs
      codeblob = state.make_blob(code.size());
      std::copy(code.begin(), code.end(), codeblob->data);
      p->code = codeblob;

      // Free variable handling
      localfreevars = state.make_blob<unsigned>(local_free_variable_count);
      upvals = state.make_blob<UpvalLoc>(upvalue_count);
      size_t freevar_i = 0, upvalue_i = 0;
      for(size_t i = 0; i != free_variables.size(); i++) {
        if(free_variables[i].lexical_level == 0)
          localfreevars->blob_set<unsigned>(freevar_i++, free_variables[i].index);
        else {
          // Note whether the variable is local to the function that
          // will be creating the closure
          UpvalLoc l(free_variables[i].lexical_level == 1, free_variables[i].index);
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

  // Return that properly unwinds the stack

  // - Closes over upvalues so they can still be accessed after this function's
  // frame is gone

  // - Pops the function's frame
#define VM_RETURN(x) { \
  for(size_t i = 0; i != prototype->local_free_variable_count; i++) { \
    f.upvalues[i]->upvalue_close(); \
  } \
  vm_frames.pop_back(); \
  vm_depth--; \
  Value* res = (x); \
  return res; }

  // Check for exceptions and unwind the stack if necessary
#define VM_CHECK(x) \
  if(x->active_exception()) { \
    VM_RETURN(x); \
  }

  size_t vm_depth;

  // A frame containing information that needs to be tracked by the garbage collector
  struct VMFrame {
    VMFrame(Prototype* p_, Closure* c_, Value** stack_, Value** locals_):
      p(p_), c(c_), stack(stack_), locals(locals_), upvalues(0), si(0)
      {}
    // Need to save pointers to the prototype and closure so they're considered roots
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

  // Attempt to apply non-function eg (#t)
  // Apply bytecode function
  Value* apply(Prototype* prototype, Closure* closure, size_t argc, Value* args[]) {
    vm_depth++;
    PIP_VM_MSG("entering function ");
    // Get pointer to code
    // NOTE: could be moved if moving garbage collector is ever used
    unsigned char* bc = (unsigned char*)(prototype->code->data);

    // Set up call frame
    VMFrame f(prototype, closure, 
        static_cast<Value**>(alloca(prototype->stack_max * sizeof(Value*))),
        static_cast<Value**>(alloca(prototype->locals * sizeof(Value*))));
    // Clear local storage (necessary because garbage collector might look at them even before they are set)
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

    // If we're not done yet, either we've received too many arguments or this is a variable arity function
    if(i != argc) {
      if(prototype->prototype_variable_arity()) {
        return make_exception(State::S_PIP_EVAL, "variable arity not supported yet");
      } else {
        VM_RETURN(arity_error());
      }
    }

    // Evaluate procedure
    unsigned ip = 0;
    while(1) {
      switch(bc[ip++]) {
        case OP_PUSH_IMMEDIATE:
          f.stack[f.si++] = (Value*)(ptrdiff_t) bc[ip++]; 
          continue;
        case OP_PUSH_CONSTANT:
          f.stack[f.si++] = f.p->constants->vector_ref(bc[ip++]);
          continue;
        case OP_GLOBAL_SET: {
          std::cout << "YEAH " << f.stack[f.si-1] << ' ' << f.stack[f.si-2] << ' ' << f.si << std::endl;
          Symbol* k = PIP_CAST(Symbol, f.stack[f.si-1]);
          f.si--;
          std::cout << "READING FROM :  " << f.si << std::endl;
          Value* v = f.stack[--f.si];
          std::cout << f.si << std::endl;
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
          unsigned char index = bc[ip++];
          Value* val = f.stack[--f.si];
          PIP_VM_VMSG("local-set " << (unsigned) index << " " << val);
          f.locals[index] = val;
          continue;
        }
        case OP_LOCAL_GET: {
          // Get the local index
          unsigned char index = bc[ip++];
          PIP_VM_VMSG("local-get " << (unsigned)index);
          f.stack[f.si++] = f.locals[index];
          continue;
        }
        case OP_UPVALUE_GET: {
          // Get upvalue index
          unsigned char index = bc[ip++];
          PIP_VM_VMSG("upvalue-get " << (unsigned)index);
          f.stack[f.si++] = closure->upvalues->data[index]->upvalue();
          continue;
        }
        case OP_UPVALUE_SET: {
          // Get upvalue index
          unsigned char index = bc[ip++];
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
//            vector_append(upvalues, f.upvalues[i]);
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
        case OP_APPLY: {
          // Retrieve argument count
          unsigned char argc = bc[ip++];
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
        default: {
          std::cout << (int)bc[ip-1] << std::endl;
          assert(!"bad instruction");
        }
      }
    }
  }

  // Safe apply which also unwraps closures
  Value* apply(Value* function, size_t argc, Value* args[]) {
    switch(function->get_type()) {
      case PROTOTYPE:
        return apply(PIP_CAST(Prototype, function), 0, argc, args);
      case CLOSURE:
        return apply(PIP_CAST(Prototype, PIP_CAST(Closure, function)->prototype), PIP_CAST(Closure, function), argc, args);
      default:
        PIP_ASSERT(!"bad application");
        break;
    }
    return PIP_UNSPECIFIED;
  } 

#undef VM_RETURN
#undef VM_CHECK
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
      return os << "#<exception " << x->exception_tag() << ' ' << x->exception_message() << '>';
    }
    case PROTOTYPE: return os << "#<prototype>";
    case UPVALUE: return os << "#<upvalue " << (x->upvalue_closed() ? "(closed) " : "") << " " << x->upvalue() << ')' << std::endl;
    default:
      return os << "#<unknown value " << (size_t) x << " type: " << x->get_type() << ">";
  }
  return os;
}

} // pip

using pip::operator<<;

#endif
