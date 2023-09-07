TOOL_NAME := peadae
TOOL_PATH := ../../build
TOOL_COMPILATION_ARGS := --cse

BUILD_PEAD_COMMAND := ${TOOL_PATH}/${TOOL_NAME} ${TOOL_COMPILATION_ARGS} --jac-spec-absolute
BUILD_AD_COMMAND := ${TOOL_PATH}/${TOOL_NAME} ${TOOL_COMPILATION_ARGS} --disable-peval

CAUER_10_EXE := cauer_10-ad.exe cauer_10-0.exe cauer_10-1.exe cauer_10-2.exe
CAUER_10_MC := $(CAUER_10_EXE:.exe=.mc)

CAUER_100_EXE := cauer_100-ad.exe cauer_100-0.exe cauer_100-1.exe cauer_100-2.exe
CAUER_100_MC := $(CAUER_100_EXE:.exe=.mc)

PENDULUM_EXE := pendulum-ad.exe pendulum-0.exe pendulum-1.exe pendulum-2.exe pendulum-3.exe
PENDULUM_MC := $(PENDULUM_EXE:.exe=.mc)

FURUTA_EXE := furuta-ad.exe furuta-0.exe furuta-1.exe furuta-2.exe furuta-3.exe furuta-4.exe furuta-5.exe furuta-6.exe furuta-7.exe furuta-8.exe furuta-9.exe furuta-10.exe
FURUTA_MC := $(FURUTA_EXE:.exe=.mc)

SIM_ARGS := --output-only-last --step-size 1 --rtol 1.e-5 --atol 1.e-7

LABEL := B
RUNS := 25
WARMUP := 5
SLEEP := 5

# 1: experiment name
# 2: simulation interval
# 3: configurations
# 4: name
define runhyperfine
	hyperfine 'if taskset -c 0 ./$(1)-{n}.exe ${SIM_ARGS} --interval $(2); then sleep ${SLEEP};  else exit 1; fi' --parameter-list n $(3) --warmup ${WARMUP} --runs ${RUNS} --export-json ${LABEL}-results-$(4).json
endef

.PHONY: clean cauer-10 cauer_100 pendulum furuta bench all

all: cauer-10 cauer-100 pendulum furuta

cauer-10: $(CAUER_10_EXE) $(CAUER_10_MC)

cauer-100: $(CAUER_100_EXE) $(CAUER_100_MC)

pendulum: $(PENDULUM_EXE) $(PENDULUM_MC)

furuta: $(FURUTA_EXE) $(FURUTA_MC)

bench: bench-cauer-10 bench-cauer-100 bench-pendulum bench-cauer-100 bench-furuta

test_cauer_arg := 2,2,2,2
test-bench-cauer-10: cauer_10-2.exe
	$(call runhyperfine,cauer_10,10000,$(test_cauer_arg),cauer_10_test)

# cauer_arg := ad,0,1,2
cauer_arg := 2,1,0,ad
bench-cauer-10: $(CAUER_10_EXE)
	$(call runhyperfine,cauer_10,10000,$(cauer_arg),cauer_10)

bench-cauer-100: $(CAUER_100_EXE)
	$(call runhyperfine,cauer_100,1000,$(cauer_arg),cauer_100)

pendulum_arg := ad,0,1,2,3
bench-pendulum: $(PENDULUM_EXE)
	$(call runhyperfine,pendulum,10000,$(pendulum_arg),pendulum)

furuta_arg := ad,0,1,2,3,4,5,6,7,8,9,10
bench-furuta: $(FURUTA_EXE)
	$(call runhyperfine,furuta,1000,$(furuta_arg),furuta)

%.exe: %.mc
	mi compile --output $@ $<

%-0.mc: %.dae
	$(BUILD_PEAD_COMMAND) 0 $< > $@

%-1.mc: %.dae
	$(BUILD_PEAD_COMMAND) 1 $< > $@

%-2.mc: %.dae
	$(BUILD_PEAD_COMMAND) 2 $< > $@

%-3.mc: %.dae
	$(BUILD_PEAD_COMMAND) 3 $< > $@

%-4.mc: %.dae
	$(BUILD_PEAD_COMMAND) 4 $< > $@

%-5.mc: %.dae
	$(BUILD_PEAD_COMMAND) 5 $< > $@

%-6.mc: %.dae
	$(BUILD_PEAD_COMMAND) 6 $< > $@

%-7.mc: %.dae
	$(BUILD_PEAD_COMMAND) 7 $< > $@

%-8.mc: %.dae
	$(BUILD_PEAD_COMMAND) 8 $< > $@

%-9.mc: %.dae
	$(BUILD_PEAD_COMMAND) 9 $< > $@

%-10.mc: %.dae
	$(BUILD_PEAD_COMMAND) 10 $< > $@

%-ad.mc: %.dae
	$(BUILD_AD_COMMAND) $< > $@

clean:
	rm -rf *.exe *.mc
