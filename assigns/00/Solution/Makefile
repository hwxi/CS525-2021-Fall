######

PATSCC=patscc

######

test: assign00_sol; ./assign00_sol

assign00_sol: assign00_sol.dats; $(PATSCC) -o $@ assign00_sol.dats

######

clean:: ; rm -f *~
clean:: ; rm -f *_?ats.c
clean:: ; rm -f *_?ats.o

cleanall:: clean
cleanall:: ; rm assign00_sol
