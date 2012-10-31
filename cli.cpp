// cli.cpp - odd command line interface
#include <stdlib.h>

#include "odd.hpp"

using namespace odd;

#include "test/runtime.cpp"

static const char help[] = \
  "commands: \n" \
  "  -r <file>           print s-expressions from file\n" \
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

int main(int argc, char** argv) {
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
    }
  }

  return EXIT_SUCCESS;
}
