CC ?= gcc

all: kilo

kilo: kilo.c
	$(CC) -Os -pipe $(CFLAGS) -o kilo kilo.c -Wall -W -pedantic -std=c99

clean:
	rm kilo
