CC ?= gcc

all: kilo

kilo: kilo.c
	$(CC) -Os -pipe -g $(CFLAGS) -o kilo kilo.c -Wall -W -pedantic -std=c99 -fwhole-program

clean:
	rm kilo
