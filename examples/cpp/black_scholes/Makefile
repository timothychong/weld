CC=clang++
LDFLAGS=-L../../../target/release/
LIBS=-lweld

.PHONY: all clean

all:
	${CC} -O3 ${LDFLAGS} ${LIBS} black_scholes.cpp -o run

clean:
	rm -rf run

