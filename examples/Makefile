TOOL_NAME=eoocore

TOOL_COMMAND := ../build/${TOOL_NAME}

OKS := rlc.dae.OK
EXES := $(OKS:.OK=.exe)
DAES := $(OKS:.OK=.mc)

.PHONY: test clean

test: $(OKS) $(EXES) $(DAES)

%.OK: %.out
	@f=$<; \
	echo -n $$f; \
	if cat $$f $${f%}.expected | ../dae/examples/compare; then \
		touch $@; \
		echo " OK"; \
	else \
		echo " FAIL"; \
	fi \

%.exe: %.mc
	@mi compile --output $@ $<

%.out: %.exe
	@./$< | tail -n 1 > $@

%.out.expected: %.out
	mv $< $@

%.dae.mc: %.eoo.mc
	@$(TOOL_COMMAND) $< > $@

clean:
	rm -rf *.exe *.out *.dae.mc *.OK
