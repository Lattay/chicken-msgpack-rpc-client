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

.PHONY: all unit test integration clean

all: msgpack-rpc-client.so

install: all
	chicken-install
	make clean

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

integration: test/tests.scm msgpack-rpc-client.so
	$(CSC) $(CSC_OPTIONS) $< -o run-test
	./run-test

msgpack-rpc-client.so: src/msgpack-rpc-client.scm
	$(CSC) $(CSC_OPTIONS) -s -j msgpack-rpc-client -o $@ $<
	$(CSC) $(CSC_OPTIONS) msgpack-rpc-client.import.scm -dynamic

clean:
	rm -f test/*.o *.o run-test unit-* *.c test/*.c *.so *.import.scm src/*.c src/*.so
	rm -f *.*.sh *.link
