CXXFLAGS := -O0 -Wall -g3 -fno-rtti -fno-exceptions 
CFLAGS := -O0 -g3
CPPFLAGS := -DODD_VERSION='"$(shell git rev-parse --short HEAD)"'
LDFLAGS := 
CXX := clang++

-include site.mk

all: odd

%.o: %.c
	@echo -n ' CC  ';
	$(strip $(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<)

odd: cli.cpp vendor/linenoise/linenoise.o odd.hpp test/runtime.cpp 
	@echo -n ' LD  ';
	$(strip $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ cli.cpp vendor/linenoise/linenoise.o $(LDFLAGS))

.PHONY: clean cloc

clean:
	rm -rf $(wildcard odd *.o odd.dSYM)

cloc:
	cloc odd.hpp
	wc -l odd.hpp
