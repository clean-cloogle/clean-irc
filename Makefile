CLM:=clm

override CLMFLAGS+=-nt

CLMLIBS:=\
	-IL Platform\
	-IL TCPIP\
	-I ./libcloogle\
	-I ./clean-selectloop/libraries

BINARIES:=IRC IRCBot cloogleirc #test

all: $(BINARIES)

%: %.icl $(wildcard */*.[id]cl *.[id]cl)
	$(CLM) $(CLMLIBS) $(CLMFLAGS) $(basename $<) -o $@

clean:
	$(RM) -r $(BINARIES) Clean\ System\ Files
