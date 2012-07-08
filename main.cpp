#include "pip.hpp"

using namespace pip;

int main(void) {
  Value* consts[] = { PIP_FALSE, PIP_TRUE, PIP_NULL, PIP_EOF };
  for(size_t i = 0; i != sizeof(consts) / sizeof(Value*); i++) {
    PIP_ASSERT(consts[i]->constantp());
    PIP_ASSERT(!consts[i]->fixnump());
    PIP_ASSERT(!consts[i]->pointerp());
    PIP_ASSERT(consts[i]->immediatep());
  }

  ptrdiff_t nums[] = { -4, -3, -1, 0, 1, 3, 4, 12345 };
  for(size_t i = 0; i != sizeof(nums) / sizeof(ptrdiff_t); i++) {
    Value* num = Value::make_fixnum(nums[i]);
    PIP_ASSERT(!num->constantp());
    PIP_ASSERT(num->fixnump());
    PIP_ASSERT(!num->pointerp());
    PIP_ASSERT(num->immediatep());
    PIP_ASSERT(num->fixnum_value() == nums[i]);
  }

  State* state = new State;

  Pair* p = state->cons(PIP_TRUE, PIP_TRUE);

  delete state;
  return 0;
}
