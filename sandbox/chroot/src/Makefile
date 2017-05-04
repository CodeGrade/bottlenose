
BINARY=sandbox
OBJS=$(shell echo *.c | perl -npE 's/\.c/.o/g')
HDRS=$(shell echo *.h | perl -npE 's/\*\.h//g')

CC=gcc -g -Wall -std=gnu99
LDFLAGS=

all: $(BINARY)

$(BINARY): $(OBJS)

$(OBJS): $(HDRS) Makefile

clean:
	rm -f $(BINARY) *.o *~

install:
	sudo ./install.sh
