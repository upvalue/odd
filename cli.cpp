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
  "  -r <file>           convert file to s-expressions and print\n" \
  "  -c <file>           compile file to bytecode\n" \
  "  -C <file> <output>  compile file to bytecode with specific output file\n" \
  "  -I <file>           dump image to file\n" \
  "  -T                  run built-in runtime test suite\n" \
  "  -h                  print this help message\n" \
  "  -R                  open a REPL\n" \
  "  -Z                  toggle tail call optimization\n" \
  "  -V                  toggle runtime trace (noisy)\n" \
  "  -v                  give version information\n" \
  "behavior:\n" \
  "  if no arguments are given, odd will open a REPL\n"\
  "  if filenames are given, odd will evalute the files and exit\n"\
  "  if other commands are given, odd will execute all commands and exit\n";

static void print_help(char** argv) {
      std::cout << "usage: " << argv[0] << " [options] [files]" << std::endl
        << help << std::endl;
}

// Convert internal module names to normal names
// #scheme#base => scheme.base
void print_module_name(std::ostream& ss, Value* x) {
  String* n = static_cast<String*>(x);
  const char* d = n->string_data();
  for(size_t i = 1; i < n->string_length(); i++) {
    ss << (d[i] == '#' ? '.' : d[i]);
  }
}

void repl() {
  char *line = 0;
  size_t linenumber = 0;
  
  // Well this is horrifying
  State& state = *::state;

  State::Compiler cc(state, NULL);
  cc.enter_module(state.table_get(*(state.user_module), state.global_symbol(State::S_MODULE_NAME)));

  Handle<Symbol> module_name(state, (Symbol*) state.table_get(*(cc.env), state.global_symbol(State::S_MODULE_NAME)));

  while(1) {
    cc.clear();

    // I tried writing directly to std::cout but it appears to cause some odd behavior in conjunction with linenoise
    std::ostringstream prompt;
    print_module_name(prompt, *module_name);
    prompt << ' ' << ++linenumber << "> ";

    line = linenoise(prompt.str().c_str());
    if(!line) break;

    std::ostringstream sd;
    sd << "repl:";
    print_module_name(sd, *module_name);
    unsigned file = state.register_string(sd.str(), line);

    std::stringstream ln;
    ln << std::noskipws;
    ln << line;

    free(line);

    State::Reader reader(state, ln, file);
    reader.line = linenumber;
    
    Value* x = 0, *check = 0;
    Prototype* proto = 0;
    ODD_FRAME(x, check, proto);
    bool success = true;
    while(1) {
      x = reader.read();
      if(x == ODD_EOF) break;
      if(x->active_exception()) {
        std::cerr << "Reader error: " << x->exception_message()->string_data() << std::endl;
        success = false;
        break;
      }
      check = cc.compile(x);
      if(check->active_exception()) {
        std::cerr << "Compiler error: " << check->exception_message()->string_data() << std::endl;
        success = false;
        break;
      }
    }
    if(success) { 
      proto = cc.end();
      x = state.apply(proto, 0, 0);
      if(x != ODD_UNSPECIFIED) 
        std::cout << x << std::endl;
    }
    module_name = (Symbol*) state.table_get(*cc.env, state.global_symbol(State::S_MODULE_NAME));
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
          case 'V': state->trace = !state->trace; continue;
          case 'Z': state->optimize_tail_calls = !state->optimize_tail_calls; continue;
          case 'v': std::cout << "odd 0.1 " << ODD_VERSION << std::endl; continue;
          default:
            std::cerr << "!!! unknown option " << argv[i] << std::endl;
            print_help(argv);
            return EXIT_FAILURE;
        }
      }
    } else {
      // File name
      Value* chk = state->load_file(arg.c_str());
      if(chk->active_exception()) {
        state->print_stack_trace(std::cout);
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
