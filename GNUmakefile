CXXFLAGS := -O3 -g3 -fno-rtti -fno-exceptions #-DPIP_DEBUG
LDFLAGS := 

ifneq ($(shell uname -s),Darwin)
	CXXFLAGS := $(CXXFLAGS) -std=c++11
endif

-include site.mk

all: pip

pip: cli.cpp pip.hpp test/tests.cpp
	@echo -n ' LD  ';
	$(strip $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $< $(LDFLAGS))

.PHONY: clean cloc update-s7 microgue.c

clean:
	rm -f $(wildcard pip *.o)
