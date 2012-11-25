CXXFLAGS := -O2 -Wall -g3 -fno-rtti -fno-exceptions 
LDFLAGS := 

-include site.mk

all: odd

odd: cli.cpp odd.hpp test/runtime.cpp
	@echo -n ' LD  ';
	$(strip $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $< $(LDFLAGS))

.PHONY: clean cloc

clean:
	rm -rf $(wildcard odd *.o odd.dSYM)

cloc:
	cloc odd.hpp
	wc -l odd.hpp
