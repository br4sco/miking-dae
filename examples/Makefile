CORE_TOOL_NAME=eoocore
CORE_TOOL_COMMAND := ../build/${CORE_TOOL_NAME}

TOOL_NAME=eoo
TOOL_COMMAND := ../build/${TOOL_NAME}

OKS := rlc_eoocore.OK cauer_eoocore.OK rlc_eoo.OK cauer_eoo.OK cps_eoo.OK inertia_eoo.OK dcmotor_eoo.OK
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

%_eoocore.mc: %.eoocore.mc
	@$(CORE_TOOL_COMMAND) $< > $@

%_eoo.mc: %.eoo
	@cat lib.eoo $< > src.eoo;
	@$(TOOL_COMMAND) --debug-backend src.eoo > $@;
	@rm src.eoo

clean:
	rm -rf *.exe *.out *_eoocore.mc *_eoo.mc src.eoo *.OK
