MODELS := harmonic.dae pendulum.dae lrc.dae
ACTUALS := $(MODELS:.dae=-10.out) $(MODELS:.dae=-05.out) $(MODELS:.dae=-03.out) $(MODELS:.dae=-00.out) $(MODELS:.dae=-numjac.out) pendulum-3d-numjac.out

.PHONY: test clean

test: $(ACTUALS)
	@for f in $(ACTUALS); do \
		echo -n $$f; \
		if cat $$f $${f%-*}.out.expected | ./compare; then \
			echo " OK"; \
		else \
			echo " FAIL"; \
		fi \
	done
	@rm -rf *.out

%.exe: %.mc
	@mi compile --output $@ $<

%.out: %.exe
	@./$< 10 1 | tail -n 1 > $@

%.out.expected: %.dae
	../peadae.exe --numeric-jac $< > $<.mc && mi compile --output $<.exe $<.mc && ./$<.exe 10 1 | tail -n 1 > $@

expected: $(MODELS:.dae=.out.expected) pendulum-3d.out.expected

clean:
	rm -rf *.exe *.out *.mc

%-10.mc: %.dae
	@../peadae.exe --jac-spec-threshold 1 $< > $@

%-05.mc: %.dae
	@../peadae.exe --jac-spec-threshold 0.5 $< > $@

%-03.mc: %.dae
	@../peadae.exe --jac-spec-threshold 0.3 $< > $@

%-00.mc: %.dae
	@../peadae.exe --jac-spec-threshold 0 $< > $@

%-numjac.mc: %.dae
	@../peadae.exe --numeric-jac $< > $@

%-00-debug.mc: %.dae
	@../peadae.exe --debug-compilation --debug-runtime --jac-spec-threshold 0 $< > $@
