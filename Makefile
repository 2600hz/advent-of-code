SOURCES = $(wildcard src/*.erl)
MODULES = $(foreach module,$(SOURCES),$(shell basename $(module) .erl))
BEAMS = $(sort $(foreach module,$(SOURCES),ebin/$(shell basename $(module) .erl).beam))

.PHONY: compile
compile: ebin priv $(BEAMS)

.PHONY: run
run: compile
	@./run.escript $(MODULES)

ebin:
	@mkdir ebin/

priv:
	@mkdir priv/

ebin/%.beam: src/%.erl
	$(info compiling $<)
	@erlc -o ebin/ $<

run-%: ebin/%.beam
	@./run.escript $*
