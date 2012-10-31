// test/runtime.cpp - basic internals tests
// included in cli.cpp

static void test_invariants(State& state) {
  Value* consts[] = { ODD_FALSE, ODD_TRUE, ODD_NULL, ODD_EOF };
  for(size_t i = 0; i != sizeof(consts) / sizeof(Value*); i++) {
    assert(consts[i]->constantp());
    assert(!consts[i]->fixnump());
    assert(!consts[i]->pointerp());
    assert(consts[i]->immediatep());
  }

  ptrdiff_t nums[] = { -4, -3, -1, 0, 1, 3, 4, 12345 };
  for(size_t i = 0; i != sizeof(nums) / sizeof(ptrdiff_t); i++) {
    Value* num = Value::make_fixnum(nums[i]);
    assert(!num->constantp());
    assert(num->fixnump());
    assert(!num->pointerp());
    assert(num->immediatep());
    assert(num->fixnum_value() == nums[i]);
  }
}

static void test_gc_1(State& state) {
  std::cout << "!! test_gc_1" << std::endl;
  // Test garbage collection
  const char* str = "string", *str2 = "string2";
  String *string1 = 0, *string2 = 0;
  Blob* blob = NULL, *blob2 = NULL;
  Pair* pair = NULL;
  ODD_FRAME(pair, blob, blob2, string1, string2);

  size_t len = sizeof(str) / sizeof(char);
  size_t len2 = len + 1;

  blob = state.make_blob(len);
  blob2 = state.make_blob(len2);
  string1 = state.make_string(str);
  string2 = state.make_string(str2);
  pair = state.cons(blob, blob2);

  strncpy(blob->data, str, len);
  strncpy(blob2->data, str2, len2);

  state.collect();
  state.collect();
  state.collect();

  assert(state.markedp(blob));
  assert(state.markedp(blob2));
  assert(state.markedp(pair));

  // allocate some random stuff
  blob2 = state.make_blob(8);
  blob2 = state.make_blob(16);
  blob2 = state.make_blob(24);
  blob2 = state.make_blob(25);

  assert(strcmp(str, blob->data) == 0);
  blob2 = ODD_CAST(Blob, pair->cdr);
  assert(blob2->get_type() == BLOB);
  assert(strcmp(str2, blob2->data) == 0);
  assert(strcmp(str, string1->string_data()) == 0);
  assert(strcmp(str2, string2->string_data()) == 0);
}

static void test_symbols(State& state) {
  std::cout << "!! test_symbols" << std::endl;
  Symbol *sym1 = 0, *sym2 = 0, *sym3 = 0;
  Handle<Symbol> sym4(state);
  ODD_FRAME(sym1, sym2, sym3);

  sym1 = state.make_symbol("symble");
  sym2 = state.make_symbol("symble");
  sym3 = state.make_symbol("simple");
  sym4 = state.make_symbol("symble");

  assert(state.markedp(sym1));
  assert(state.markedp(sym2));
  assert(state.markedp(sym3));
  assert(state.markedp(*sym4));

  state.collect();
  state.collect();
  state.collect();
  
  assert(sym1 == sym2);
  assert(sym1 != sym3);
  assert(sym1 == *sym4);
  assert(state.markedp(sym1));
  assert(state.markedp(sym2));
  assert(state.markedp(sym3));
  assert(state.markedp(*sym4));
}

static void test_vectors(State& state) {
  std::cout << "!! test_vectors" << std::endl;
  Vector* v = 0;
  ODD_FRAME(v);
  v = state.make_vector();

  //state.collect();

  for(int i = 0; i != 10; i++) {
    state.vector_append(v, Value::make_fixnum(i));
  }

  state.collect();

  for(int i = 0; i != 10; i++) {
    assert(v->vector_ref(i) == Value::make_fixnum(i));
  }

  std::cout << v << std::endl;
}

static void test_tables(State& state) {
  Table* table = NULL;
  String* str = NULL;
  ODD_FRAME(table, str);
  std::cout << "!! test_tables" << std::endl;

  table = state.make_table(6); 

  for(int i = 0; i != 100; i++) {
    bool found = false;
    state.table_insert(table, Value::make_fixnum(i), Value::make_fixnum(i));

    assert(state.table_get(table, Value::make_fixnum(i), found) == Value::make_fixnum(i));
    assert(found);
  }

  str = state.make_string("hello");

  state.table_insert(table, str, Value::make_fixnum(12345));
  bool found = false;
  assert(state.table_get(table, str, found) == Value::make_fixnum(12345));
  assert(found);

  // TODO: deletions
}

static void test_reader(State& state) {
  std::cout << "!! test_reader" << std::endl;
  unsigned file = state.register_file("test/read.odd");
  std::fstream fs("test/read.odd");
  State::Reader reader(state, fs, file);
  Value* x = 0;
  ODD_FRAME(x);
  while(true) {
    x = reader.read();
    if(x == ODD_EOF) break;
    std::cout << x << std::endl;
  }
}

Value* eval_string(State& state, const std::string& string) {
  unsigned file = state.register_string("string", string);
  State::Compiler cc(state, NULL);
  std::stringstream ss;
  ss << std::noskipws;
  ss << string;
  State::Reader reader(state, ss, file);
  Value* x = 0;
  Value* check = 0;
  Prototype* p = 0;
  ODD_FRAME(x, p, check);
  std::cout << "!! compile_string: " << string << std::endl;
  while(true) {
    x = reader.read();
    if(x == ODD_EOF) break;
    check = cc.compile(x);
    if(check != ODD_FALSE) {
      std::cerr << check << std::endl;
      assert(0);
    }
  }
  p = cc.end();
  x = state.apply(p, 0, 0);
  return x;
}

void test_eval(State& state, const std::string& string, Value* result) {
  Value* x = eval_string(state, string);
  std::cout << "!! test_compiler: " << string <<  " => " << x << std::endl;
  assert(x == result);
}

void test_eval(State& state, const std::string& string, Type type) {
  Value* x = eval_string(state, string);
  std::cout << "!! test_compiler (result type): " << string << " => " << type << std::endl;
  assert(x->get_type() == type);
}

// test_eval for weird, one-off assertions
template <class success_functor_t> void test_eval(State& state, const std::string& string, success_functor_t& success_functor) {
  Value* x = eval_string(state, string);
  std::cout << "!! test compiler: " << string << " => " << success_functor.description << std::endl;
  assert(success_functor(x));
}

struct named_lambda {
  named_lambda() {
    description = "#<prototype> has name 'something'";
  }
  
  bool operator()(Value* x) {
    if(x->get_type() != PROTOTYPE) return false;
    Prototype* p = static_cast<Prototype*>(x);
    if(!p->name) return false;
    return strcmp(p->name->string_data(), "something") == 0;
  }

  const char* description;
};

void test_compiler(State& state) {
  std::cout << "!! test_compiler" << std::endl;
  test_eval(state, "#t", ODD_TRUE);
  test_eval(state, "#f", ODD_FALSE);
  // Global variable access
  test_eval(state, "(define x #t) x", ODD_TRUE);

  // Function compilation
  test_eval(state, "(lambda () #t) ", PROTOTYPE);
  // Simple application
  test_eval(state, "((lambda () #t))", ODD_TRUE);

  // Argument function and application
  test_eval(state, "((lambda (x) x) #t)", ODD_TRUE);
  // Set!
  test_eval(state, "(define z #f) (set! z #t) z", ODD_TRUE);
  test_eval(state, "((lambda (x) (set! x #t) x) #f)", ODD_TRUE);
  // Closures
  test_eval(state, "((lambda (x) ((lambda () x))) #t)", ODD_TRUE);
  // 2-level Closure
  test_eval(state, "((lambda (x) ((lambda () ((lambda () x))))) #t)", ODD_TRUE);
  //test_eval(state, "((lambda (x) ((lambda () ((lambda () x)))) #t))", ODD_TRUE);
  // Define lambda shortcut
  test_eval(state, "(define (name) #t) (name)", ODD_TRUE);
  // If
  test_eval(state, "(if #t #t #f)", ODD_TRUE);
  // One-arm if
  test_eval(state, "(if #t #t)", ODD_TRUE);
  test_eval(state, "(if #f #f)", ODD_UNSPECIFIED);
  // Begin
  test_eval(state, "(begin)", ODD_UNSPECIFIED);
  
  test_eval(state, "(begin #f #f #t)", ODD_TRUE);
  // Tail call
  test_eval(state, "((lambda () ((lambda () #t))))", ODD_TRUE);
  // Quote
  test_eval(state, "(quote #t)", ODD_TRUE);

  // Built-in functions
  test_eval(state, "(car (quote (#t)))", ODD_TRUE);

  // Test that defining lambdas properly detects their names
  named_lambda named_lambda_functor;
  test_eval<named_lambda>(state, "(define (something) #t) something", named_lambda_functor);
  test_eval<named_lambda>(state, "(define something (lambda () #t)) something", named_lambda_functor);

  // Stack management
  test_eval(state, "((lambda () #t #t #t))", ODD_TRUE);

  // define-syntax
  test_eval(state, "(define-syntax hello (er-macro-transformer (lambda (x r c) #t))) (hello)", ODD_TRUE);

  // test define-library
  test_eval(state, "(define-library (one two) (begin #t))", ODD_TRUE);

  // test runtime provided eval function
  assert(state.eval(ODD_TRUE, (*state.core_env)) == ODD_TRUE);
}

void run_test_suite(State& state) {
  std::cout << "!! sizeof(State) " << sizeof(State) << " [" << FriendlySize(sizeof(State)) << "]" << std::endl;
  state.collect_before_every_allocation = true;
  test_invariants(state);
  test_gc_1(state);
  test_symbols(state);
  test_vectors(state);
  test_tables(state);
  test_reader(state);
  //test_compiler(state);

  std::cout << "!! collections: " << state.collections << " heap size: " << FriendlySize(state.heap_size) << std::endl;
}

void test() {
  State* state = new State;
  
  run_test_suite(*state);
  state->compact();
  run_test_suite(*state);
  state->compact();
  run_test_suite(*state);

  // print symbol table
  Table * tbl = ODD_CAST(Table, state->core_env->cdr);
  for(size_t i = 0; i != tbl->chains->length; i++) {
    Value* cell = tbl->chains->data[i];
    while(cell->get_type() == PAIR) {
      std::cout << cell->caar() << std::endl;
      cell = cell->cdr();
    }
  }

  delete state;
}
