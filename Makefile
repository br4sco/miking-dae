CORE_TOOL_NAME=eoocore
TOOL_NAME=eoo

SRCS := $(shell find . -name "*.mc" -a ! -name "eoocore.mc" -a ! -name "ast_gen.mc" -a ! -wholename "./examples/*" ! -wholename "./dae/*" ! -wholename "./legacy/*")
TESTS := $(SRCS:.mc=.test)
TESTBINS := $(SRCS:.mc=.test.exe.run)

.PHONY: test test-examples clean

all: build/${CORE_TOOL_NAME} build/${TOOL_NAME}

build/${CORE_TOOL_NAME}: ${CORE_TOOL_NAME}.exe
	mkdir -p build
	mv ${CORE_TOOL_NAME}.exe build/${CORE_TOOL_NAME}

build/${TOOL_NAME}: ${TOOL_NAME}.exe
	mkdir -p build
	mv ${TOOL_NAME}.exe build/${TOOL_NAME}

test: $(TESTS)

test-compiled: $(TESTBINS)

test-examples: build/${CORE_TOOL_NAME}
	$(MAKE) -C examples test

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

${CORE_TOOL_NAME}.exe: $(SRCS)

${TOOL_NAME}.exe: $(SRCS)

ast_gen.mc: ast.syn
	mi syn ast.syn ast_gen.mc

clean:
	$(MAKE) -C examples clean
	rm -rf *.exe build
