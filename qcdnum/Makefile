#!/bin/bash

CXX = g++
FC  = gfortran

CXXFLAGS += -O3 -fPIC -std=c++11

# APFEL++
APFELPPINCS = $(shell apfelxx-config --cppflags)
APFELPPLIBS = $(shell apfelxx-config --ldflags)

# QCDNUM
QCDNUMINCS = $(shell qcdnum-config --cppflags)
QCDNUMLIBS = $(shell qcdnum-config --ldflags)

# Now set up the compiler and link flags and libs
CXXFLAGS += $(APFELPPINCS) $(QCDNUMINCS)
LDFLAGS  += $(APFELPPINCS) $(QCDNUMINCS)

CLIBS += $(APFELPPLIBS) $(QCDNUMLIBS)

install : all
all : Evolution

Evolution: Evolution.o
	$(CXX) $(LDFLAGS) -o $@ $< $(CLIBS)

.SUFFIXES : .cc .o .f .c .f90

.cxx.o:
	$(CXX) $(CXXFLAGS) -c $<

.f.o:
	$(FC) $(LDFLAGS) -c $<

.f90.o:
	$(FC) $(LDFLAGS) -c $<

clean:
	rm -rf *.lo *.o *.la Evolution *~

