// test/runtime.cpp - basic internals tests
// included in cli.cpp

static void test_invariants(State& state) {
  Value* consts[] = { PIP_FALSE, PIP_TRUE, PIP_NULL, PIP_EOF };
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
  PIP_FRAME(pair, blob, blob2, string1, string2);

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
  blob2 = PIP_CAST(Blob, pair->cdr);
  assert(blob2->get_type() == BLOB);
  assert(strcmp(str2, blob2->data) == 0);
  assert(strcmp(str, string1->string_data()) == 0);
  assert(strcmp(str2, string2->string_data()) == 0);
}

static void test_symbols(State& state) {
  std::cout << "!! test_symbols" << std::endl;
  Symbol *sym1 = 0, *sym2 = 0, *sym3 = 0;
  State::Handle<Symbol> sym4(state);
  PIP_FRAME(sym1, sym2, sym3);

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
  PIP_FRAME(v);
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

static void test_reader(State& state) {
  std::cout << "!! test_reader" << std::endl;
  unsigned file = state.register_file("test/read.ss");
  std::fstream fs("test/read.ss");
  State::Reader reader(state, fs, file);
  Value* x = 0;
  PIP_FRAME(x);
  while(true) {
    x = reader.read();
    if(x == PIP_EOF) break;
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
  PIP_FRAME(x, p, check);
  std::cout << "!! compile_string: " << string << std::endl;
  while(true) {
    x = reader.read();
    if(x == PIP_EOF) break;
    check = cc.compile(x);
    if(check != PIP_FALSE) {
      std::cerr << check << std::endl;
      assert(0);
    }
  }
  p = cc.end();
  x = state.apply(p, 0, 0, 0);
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

void test_compiler(State& state) {
  std::cout << "!! test_compiler" << std::endl;
  test_eval(state, "#t", PIP_TRUE);
  test_eval(state, "#f", PIP_FALSE);
  // Global variable access
  test_eval(state, "(define x #t) x", PIP_TRUE);
  // Function compilation
  test_eval(state, "(lambda () #t) ", PROTOTYPE);
  // Simple application
  test_eval(state, "((lambda () #t))", PIP_TRUE);
  // Argument function and application
  test_eval(state, "((lambda (x) x) #t)", PIP_TRUE);
  // Set!
  test_eval(state, "(define z #f) (set! z #t) z", PIP_TRUE);
  test_eval(state, "((lambda (x) (set! x #t) x) #f)", PIP_TRUE);
  // Closures
  test_eval(state, "((lambda (x) ((lambda () x))) #t)", PIP_TRUE);
  // 2-level Closure
  test_eval(state, "((lambda (x) ((lambda () ((lambda () x))))) #t)", PIP_TRUE);
  //test_eval(state, "((lambda (x) ((lambda () ((lambda () x)))) #t))", PIP_TRUE);
  // Define lambda shortcut
  test_eval(state, "(define (name) #t) (name)", PIP_TRUE);
  // If
  test_eval(state, "(if #t #t #f)", PIP_TRUE);
  // One-arm if
  test_eval(state, "(if #t #t)", PIP_TRUE);
  test_eval(state, "(if #f #f)", PIP_UNSPECIFIED);
  // Begin
  test_eval(state, "(begin)", PIP_UNSPECIFIED);
  test_eval(state, "(begin #f #f #t)", PIP_TRUE);
}

void test() {
  State* state = new State;

  std::cout << "!! sizeof(State) " << sizeof(State) << " [" << FriendlySize(sizeof(State)) << "]" << std::endl;
  state->collect_before_every_allocation = true;
  test_gc_1(*state);
  test_symbols(*state);
  test_vectors(*state);
  test_reader(*state);
  test_compiler(*state);

  { 
    State::Compiler cc(*state, NULL);
    cc.compile(PIP_TRUE);
    Prototype* p = 0;
    {
      PIP_E_FRAME(*state, p);
      p = cc.end();
      std::cout << state->apply(p, 0, 0, 0) << std::endl;
    }
  }

  std::cout << "Collections "<< state->collections << std::endl;

  delete state;
}
