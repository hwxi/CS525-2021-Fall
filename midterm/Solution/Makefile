######
#
# A simple Makefile
#
######

PATSCC=patscc
PATSOPT=patsopt
MYATSCC=myatscc

######

lambda2_sol: ; \
$(PATSCC) -o lambda2_sol \
-DATS_MEMALLOC_LIBC ./../lambda1.dats lambda2_sol.dats

######

clean:: ; rm -f *~
clean:: ; rm -f *_dats.c
cleanall:: clean
cleanall:: ; rm -f lambda2_sol

############ end of [Makefile] ############
