######
#
# A simple Makefile
#
######

PATSCC=patscc
PATSOPT=patsopt
MYATSCC=myatscc

######

mylist_test: \
mylist_test.dats ; \
$(PATSCC) \
-DATS_MEMALLOC_LIBC \
-o mylist_test mylist_test.dats

######

clean:: ; rm -f *~
clean:: ; rm -f *_dats.c
cleanall:: clean
cleanall:: ; rm -f mylist_test

############ end of [Makefile] ############
