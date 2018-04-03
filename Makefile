CLEAN_HOME?=/opt/clean
CLM:=clm

override CLMFLAGS+=-nt

CLMLIBS:=\
	-I $(CLEAN_HOME)/lib/Platform\
	-I $(CLEAN_HOME)/lib/Platform/Deprecated/StdLib\
	-I $(CLEAN_HOME)/lib/Generics\
	-I $(CLEAN_HOME)/lib/TCPIP\
	-I $(CLEAN_HOME)/lib/Dynamics\
	-I ./libcloogle\
	-I ./clean-selectloop/libraries

BINARIES:=IRC IRCBot cloogleirc #test

all: $(BINARIES)

%: %.icl $(wildcard */*.[id]cl *.[id]cl)
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $(basename $<) -o $@

clean:
	$(RM) -r $(BINARIES) Clean\ System\ Files
