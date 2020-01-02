PREFIX=
CSC=
ifeq ($(CSC),)
ifeq ($(PREFIX),)
CSC=csc
else
CSC=$(PREFIX)/bin/csc
endif
endif

CSC_OPTIONS=

.PHONY: all unit test clean

all: msgpackrpc.so

install: all
	chicken-install
	make clean

help:
	@echo "Usage: make [PREFIX=<chicken installation prefix>] [CSC=<csc command name>] <target>"
	@echo "Available target:"
	@echo "  Test targets:"
	@echo "    test                   proceed to all tests"
	@echo "    unit                   unit testing of server and client"
	@echo ""
	@echo "  Lib targets:"
	@echo ""
	@echo "  Other targets:"
	@echo "    all                    compile all libs"
	@echo "    clean                  remove every build product"

# Development test

test: unit

unit:

msgpack-rpc.so: $(SRC)
	$(CSC) $(CSC_OPTIONS) -s -j msgpackrpc-client -o $@ $<
	$(CSC) $(CSC_OPTIONS) msgpackrpc-client.import.scm -dynamic

clean:
	rm -f test/*.o *.o run unit-* *.c test/*.c *.so *.import.scm test/run src/*.c src/*.so
	rm -f *.*.sh *.link
