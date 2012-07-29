// cli.cpp - pip command line interface

#include "pip.hpp"

using namespace pip;

#include "test/tests.cpp"

int main(int argc, char** argv) {
  test();

  return 0;
}
