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

SRC=$(wildcard src/*.scm)

.PHONY: all unit test integration install clean

all: msgpack-rpc-client.so

help:
	@echo "Usage: make [PREFIX=<chicken installation prefix>] [CSC=<csc command name>] <target>"
	@echo "Available target:"
	@echo "  Test targets:"
	@echo "    test                   proceed to all tests"
	@echo "    unit                   unit testing of server and client"
	@echo "    integration            integration testing of server and client (depends on Python components)"
	@echo ""
	@echo "  Lib targets:"
	@echo ""
	@echo "  Other targets:"
	@echo "    all                    compile all libs"
	@echo "    clean                  remove every build product"

# Development test

test: integration unit

unit:

integration: test/tests.scm install
	$(CSC) $(CSC_OPTIONS) $< -o run-test
	./run-test

stdio: test/stdio-s2c.scm test/pseudo-server.scm install
	$(CSC) $(CSC_OPTIONS) test/stdio-s2c.scm -o run-test1
	$(CSC) $(CSC_OPTIONS) test/pseudo-server.scm -o run-test2
	ncat -l -p8000 -c ./run-test1

install: $(SRC)
	chicken-install
	make clean

msgpack-rpc-client.so: src/msgpack-rpc-client.scm
	$(CSC) $(CSC_OPTIONS) -s -j msgpack-rpc-client -o $@ $<
	$(CSC) $(CSC_OPTIONS) msgpack-rpc-client.import.scm -dynamic

clean:
	rm -f test/*.o *.o run-test unit-* *.c test/*.c *.so *.import.scm src/*.c src/*.so
	rm -f *.*.sh *.link
