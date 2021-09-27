######
#
# A simple Makefile
#
######

PATSCC=patscc
PATSOPT=patsopt
#MYATSCC=myatscc

######

lambda0: \
lambda0.dats; \
$(PATSCC) -o lambda0 -DATS_MEMALLOC_LIBC lambda0.dats

######

clean:: ; rm -f *~
clean:: ; rm -f *_dats.c
cleanall:: clean
cleanall:: ; rm -f lambda0

############ end of [Makefile] ############
