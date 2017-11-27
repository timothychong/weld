UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
    CC=gcc-6
endif

ifeq ($(UNAME_S),Linux)
    CC=gcc
endif

CPPFLAGS=-O3 -march=native -std=c++11

.PHONY: all clean

all:
	${CC} tpch_q6.cpp ${CPPFLAGS} -o bench

clean:
	rm -f bench