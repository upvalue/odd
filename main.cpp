// main.cpp - odd command line interface

#include "pip.hpp"

using namespace pip;

static void test_gc_1(State& state) {
  std::cout << "!! test_gc_1" << std::endl;
  // Test garbage collection
  const char* str = "string", *str2 = "string2";
  Blob* blob = NULL, *blob2 = NULL;
  Pair* pair = NULL;
  PIP_FRAME(state, pair, blob, blob2);

  size_t len = sizeof(str) / sizeof(char);
  size_t len2 = len + 1;

  blob = state.make_blob(len);
  blob2 = state.make_blob(len2);
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
}

void test() {
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

  State* state = new State;

  test_gc_1(*state);

  delete state;
}

int main(void) {
  test();

  return 0;
}
