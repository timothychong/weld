CC=g++
LDFLAGS=-L../../../../target/release/
LIBS=-lweld
DYNFLAGS=-dynamiclib -flat_namespace

.PHONY: all clean

all:
	${CC} ${DYNFLAGS} ${LDFLAGS} ${LIBS} libtest.cc -o ../libtest.so

clean:
	rm ../libtest.so

