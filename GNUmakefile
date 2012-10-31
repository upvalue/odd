CXXFLAGS := -O2 -Wall -g3 -fno-rtti -fno-exceptions -odde 
LDFLAGS := 

ifneq ($(shell uname -s),Darwin)
	CXXFLAGS := $(CXXFLAGS) -std=c++11
endif

-include site.mk

all: odd

odd: cli.cpp odd.hpp test/runtime.cpp
	@echo -n ' LD  ';
	$(strip $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $< $(LDFLAGS))

.PHONY: clean cloc

clean:
	rm -f $(wildcard odd *.o)

cloc:
	cloc odd.hpp
	wc -l odd.hpp
