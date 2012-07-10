// main.cpp - odd command line interface

#include "pip.hpp"

using namespace pip;

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
  PIP_FRAME(state, pair, blob, blob2, string1, string2);

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
  Handle<Symbol> sym4(state);
  PIP_FRAME(state, sym1, sym2, sym3);

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

static void test_reader(State& state) {
  std::cout << "!! test_reader" << std::endl;
  unsigned file = state.register_file("test/read.ss");
  std::fstream fs("test/read.ss");
  State::Reader reader(state, fs, file);
  Value* x = 0;
  PIP_FRAME(state, x);
  while(true) {
    x = reader.read();
    if(x == PIP_EOF) break;
    std::cout << x << std::endl;
  }
}

void test() {
  State* state = new State;

  state->collect_before_every_allocation = true;
  test_gc_1(*state);
  test_symbols(*state);
  test_reader(*state);

  std::cout << "Collections "<< state->collections << std::endl;

  delete state;
}

int main(void) {
  test();

  return 0;
}
