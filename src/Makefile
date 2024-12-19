CC = gcc$(CPPFLAGS) $(CFLAGS)

# CFLAGS += -fsanitize=address -g -O1
# LDFLAGS += -fsanitize=address

jikespg:                        main
	$(CC) $(LDFLAGS) -o jikespg main.o

main: main.c
	$(CC) -c main.c

clean:
	rm -f jikespg
	rm -f globals.o
	rm -f lpgparse.o 
	rm -f main.o
