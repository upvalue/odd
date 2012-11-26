// cli.cpp - odd command line interface
#include <stdlib.h>

#include "odd.hpp"

#include "vendor/linenoise/linenoise.h"
#include "vendor/linenoise/linenoise.c"

using namespace odd;

State* state = 0;

#include "test/runtime.cpp"

static const char help[] = \
  "commands: \n" \
  "  -r <file>           convert file to s-expressions and prin;t\n" \
  "  -c <file>           compile file to bytecode\n" \
  "  -C <file> <output>  compile file to bytecode with specific output file\n" \
  "  -I <file>           dump image to file\n" \
  "  -T                  run built-in runtime test suite\n" \
  "  -h                  print this help message\n" \
  "  -R                  open a REPL\n" \
  "  -V                  give version information\n" \
  "behavior:\n" \
  "  if no arguments are given, odd will open a REPL\n"\
  "  if filenames are given, odd will evalute the files and exit\n"\
  "  if other commands are given, odd will execute all commands and exit\n";

static void print_help(char** argv) {
      std::cout << "usage: " << argv[0] << " [options] [files]" << std::endl
        << help << std::endl;
}

void repl() {
  char *line = 0;

  std::ostringstream ss; 
  State::Compiler cc(*state, NULL);

  ss << "> ";

  while((line = linenoise(ss.str().c_str())) != NULL) {
    printf("yecho\n");
    free(line);
  }
}

int main(int argc, char** argv) {
  state = new State;
  // Parse options
  for(int i = 1; i < argc; i++) {
    std::string arg(argv[i]);
    if(arg[0] == '-') {
      if(arg.length() > 1) {
        switch(arg[1]) {
          case 'h': print_help(argv); continue;
          case 'T': test(); continue;
          default:
            std::cerr << "!!! unknown option " << argv[i] << std::endl;
            print_help(argv);
            return EXIT_FAILURE;
        }
      }
    } else {
      // filename
      Value* chk = state->load_file(arg.c_str());
      if(chk->active_exception()) {
        std::cout << "ERROR: " << chk->exception_message()->string_data() << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  if(argc == 1)
    repl();

  delete state; 
  return EXIT_SUCCESS;
}
