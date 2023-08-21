MODELS_NUMERIC_JAC := harmonic.dae pendulum.dae lrc.dae body.dae pendulum-3d.dae furuta.dae
MODELS_GEN_JAC := harmonic.dae pendulum.dae lrc.dae pendulum-3d.dae furuta.dae
OKS := $(MODELS_NUMERIC_JAC:.dae=-numjac.OK) $(MODELS_GEN_JAC:.dae=-1.OK) $(MODELS_GEN_JAC:.dae=-09.OK) $(MODELS_GEN_JAC:.dae=-08.OK) $(MODELS_GEN_JAC:.dae=-07.OK) $(MODELS_GEN_JAC:.dae=-06.OK) $(MODELS_GEN_JAC:.dae=-05.OK) $(MODELS_GEN_JAC:.dae=-04.OK) $(MODELS_GEN_JAC:.dae=-03.OK) $(MODELS_GEN_JAC:.dae=-02.OK) $(MODELS_GEN_JAC:.dae=-01.OK) $(MODELS_GEN_JAC:.dae=-00.OK)

EXES := $(OKS:.OK=.exe)
MCS := $(OKS:.OK=.mc)

PEADAE_SPEC := ../peadae.exe --debug-compilation --jac-spec-threshold

.PHONY: test clean

test: $(OKS) $(EXES) $(MCS)

%.OK: %.out
	@f=$<; \
	echo -n $$f; \
	if cat $$f $${f%-*}.out.expected | ./compare; then \
		touch $@; \
		echo " OK"; \
	else \
		echo " FAIL"; \
	fi \

%.exe: %.mc
	@mi compile --output $@ $<

%.out: %.exe
	@./$< 10 1 | tail -n 1 > $@

%.out.expected: %.dae
	../peadae.exe --numeric-jac $< > $<.mc && mi compile --output $<.exe $<.mc && ./$<.exe 10 1 | tail -n 1 > $@

expected: $(MODELS:.dae=.out.expected) pendulum-3d.out.expected

clean:
	rm -rf *.exe *.out *.mc *.OK

%-1.mc: %.dae
	@$(PEADAE_SPEC) 1 $< > $@

%-09.mc: %.dae
	@$(PEADAE_SPEC) 0.09 $< > $@

%-08.mc: %.dae
	@$(PEADAE_SPEC) 0.08 $< > $@

%-07.mc: %.dae
	@$(PEADAE_SPEC) 0.07 $< > $@

%-06.mc: %.dae
	@$(PEADAE_SPEC) 0.06 $< > $@

%-05.mc: %.dae
	@$(PEADAE_SPEC) 0.05 $< > $@

%-04.mc: %.dae
	@$(PEADAE_SPEC) 0.04 $< > $@

%-03.mc: %.dae
	@$(PEADAE_SPEC) 0.03 $< > $@

%-02.mc: %.dae
	@$(PEADAE_SPEC) 0.02 $< > $@

%-01.mc: %.dae
	@$(PEADAE_SPEC) 0.01 $< > $@

%-00.mc: %.dae
	@$(PEADAE_SPEC) 0 $< > $@

%-numjac.mc: %.dae
	@../peadae.exe --debug-compilation --numeric-jac $< > $@

%-opt_numjac.mc: %.dae
	@../peadae.exe --debug-compilation --cse --constant-fold --numeric-jac $< > $@

%-00-debug.mc: %.dae
	@../peadae.exe --debug-compilation --debug-runtime --jac-spec-threshold 0 $< > $@
