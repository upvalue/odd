CXXFLAGS := -O2 -Wall -g3 -fno-rtti -fno-exceptions -DPIP_DEBUG -pipe 
LDFLAGS := 

ifneq ($(shell uname -s),Darwin)
	CXXFLAGS := $(CXXFLAGS) -std=c++11
endif

-include site.mk

all: pip

pip: cli.cpp pip.hpp test/runtime.cpp
	@echo -n ' LD  ';
	$(strip $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $< $(LDFLAGS))

.PHONY: clean cloc

clean:
	rm -f $(wildcard pip *.o)

cloc:
	cloc pip.hpp
	wc -l pip.hpp
