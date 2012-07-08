CXXFLAGS := -O2 -g3 -fno-rtti -fno-exceptions -DPIP_DEBUG
LDFLAGS := 

-include site.mk

all: pip

pip: main.cpp pip.hpp
	@echo -n ' LD  ';
	$(strip $(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@ $< $(LDFLAGS))

.PHONY: clean cloc update-s7 microgue.c

clean:
	rm -f $(wildcard main *.o)
