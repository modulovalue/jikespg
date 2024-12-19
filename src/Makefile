CC = gcc$(CPPFLAGS) $(CFLAGS)

# CFLAGS += -fsanitize=address -g -O1
# LDFLAGS += -fsanitize=address

jikespg:                        lpgparse.o main.o
	$(CC) $(LDFLAGS) -o jikespg lpgparse.o main.o

main.o:	main.c common.h
	$(CC) -c main.c

lpgparse.o: common.h lpgparse.c
	$(CC) -c lpgparse.c

clean:
	rm -f jikespg
	rm -f globals.o
	rm -f lpgparse.o 
	rm -f main.o
