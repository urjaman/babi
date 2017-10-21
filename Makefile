CC ?= gcc

all: babi

babi: babi.c
	$(CC) -Os -pipe -g $(CFLAGS) -o babi babi.c -Wall -W -pedantic -std=c99 -fwhole-program

clean:
	rm babi
