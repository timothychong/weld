CC=clang++
LDFLAGS=-L../../../target/release/
LIBS=-lweld

.PHONY: all clean

all:
	${CC} ${LDFLAGS} ${LIBS} vector_add_fpga.cpp -o run

clean:
	rm -rf run

