CC = gcc$(CPPFLAGS) $(CFLAGS)

# CFLAGS += -fsanitize=address -g -O1
# LDFLAGS += -fsanitize=address

jikespg:                        ctabs.o globals.o lpgparse.o lpgutil.o main.o mkfirst.o mkred.o mkstates.o partset.o remsp.o
	$(CC) $(LDFLAGS) -o jikespg ctabs.o globals.o lpgparse.o lpgutil.o main.o mkfirst.o mkred.o mkstates.o partset.o remsp.o

main.o:	main.c common.h
	$(CC) -c main.c

globals.o: globals.c common.h
	$(CC) -c globals.c

lpgparse.o: lpgprs.h lpgdcl.h lpgdef.h lpgact.h lpgact.c lpgsym.h common.h lpgparse.h lpgparse.c
	$(CC) -c lpgparse.c

lpgutil.o: lpgutil.c common.h
	$(CC) -c lpgutil.c

lpgact.o: lpgact.c lpgparse.h
	$(CC) -c lpgact.c

mkfirst.o: mkfirst.c common.h
	$(CC) -c mkfirst.c

mkstates.o: mkstates.c common.h
	$(CC) -c mkstates.c

mkred.o: mkred.c common.h
	$(CC) -c mkred.c

remsp.o: remsp.c common.h
	$(CC) -c remsp.c

partset.o: partset.c common.h
	$(CC) -c partset.c

ctabs.o: ctabs.c common.h
	$(CC) -c ctabs.c

clean:
	rm -f jikespg
	rm -f ctabs.o 
	rm -f globals.o
	rm -f lpgparse.o 
	rm -f lpgutil.o 
	rm -f main.o 
	rm -f mkfirst.o 
	rm -f mkred.o
	rm -f mkstates.o 
	rm -f partset.o 
	rm -f remsp.o

veryclean: clean
	rm -f lpgprs.h
	rm -f lpgdcl.h
	rm -f lpgdef.h
	rm -f lpgact.h
	rm -f lpgact.i
	rm -f lpgsym.h