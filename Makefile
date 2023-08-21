SRCS := $(shell find . -name "*.mc" -a ! -name "peadae.mc" -a ! -name "ast_gen.mc" -a ! -wholename "./examples/*")
TESTS := $(SRCS:.mc=.test)
TESTBINS := $(SRCS:.mc=.test.exe.run)

.PHONY: test test-examples watch-test clean

all: peadae.exe

test: $(TESTS)

test-compiled: $(TESTBINS)

test-examples:
	$(MAKE) test -C examples

test-all: test test-examples

%.test: %.mc
	mi --test $<
	@echo ""

%.exe.run: %.exe
	./$<
	@echo ""

%.test.exe: %.mc
	mi compile --test --output $@ $<

%.exe: %.mc
	mi compile --output $@ $<

peadae.exe: $(SRCS)

ast_gen.mc: ast.syn
	mi syn ast.syn ast_gen.mc

watch:
	find . "(" -name "*.mc" -o -name "*.syn" ")" -a ! -name "ast_gen.mc" | entr -rc make test

clean:
	rm -rf *.exe
	make clean -C examples
