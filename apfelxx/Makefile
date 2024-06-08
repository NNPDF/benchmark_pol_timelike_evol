
CXX = clang++

CXXFLAGS += -O3 -fPIC -std=c++17

# LHAPDF
LHAPDFINCS = #$(shell lhapdf-config --cppflags)
LHAPDFLIBS = #$(shell lhapdf-config --ldflags)

# APFEL++
APFELPPINCS = $(shell apfelxx-config --cppflags)
APFELPPLIBS = $(shell apfelxx-config --ldflags)

# APFEL
APFELINCS = #$(shell apfel-config --cppflags)
APFELLIBS = #$(shell apfel-config --ldflags)

# MELA
MELALIBS = #$(shell mela-config --ldflags)

# Now set up the compiler and link flags and libs
CXXFLAGS += $(LHAPDFINCS) $(APFELINCS) $(APFELPPINCS) 
LDFLAGS  += $(LHAPDFINCS) $(APFELINCS) $(APFELPPINCS) 

CLIBS += $(LHAPDFLIBS) $(APFELLIBS) $(APFELPPLIBS) $(MELALIBS)

install : all
all : EvolutionPol EvolutionTL

EvolutionPol: EvolutionPol.o
	$(CXX) $(LDFLAGS) -o $@ $< $(CLIBS)

EvolutionTL: EvolutionTL.o
	$(CXX) $(LDFLAGS) -o $@ $< $(CLIBS)

.SUFFIXES : .cc .o .f .c

.cxx.o:	 
	$(CXX) $(CXXFLAGS) -c $< 

.f.o:	 
	$(F77)  -c $< 

clean:
	rm -rf *.lo *.o *.la EvolutionPol EvolutionTL *~
