CLEAN_HOME?=/opt/clean
CLM:=clm

override CLMFLAGS+=-nt -dynamics -lat -d -nsa -nou
GCCVERSIONGTEQ6:=$(shell expr `gcc -dumpversion | cut -f1 -d.` \>= 6)
ifeq "$(GCCVERSIONGTEQ6)" "1"
	override CLMFLAGS+=-l -no-pie
endif

CLMLIBS:=\
	-I $(CLEAN_HOME)/lib/Platform\
	-I $(CLEAN_HOME)/lib/Platform/Deprecated/StdLib\
	-I $(CLEAN_HOME)/lib/Generics\
	-I $(CLEAN_HOME)/lib/TCPIP\
	-I $(CLEAN_HOME)/lib/Dynamics\
	-I ./libcloogle

BINARIES:=IRC IRCBot cloogleirc #test

all: $(BINARIES)

%: %.icl $(wildcard */*.[id]cl *.[id]cl)
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $(basename $<) -o $@

clean:
	$(RM) -r $(BINARIES) Clean\ System\ Files
