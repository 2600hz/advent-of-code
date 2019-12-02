# Stub Makefile from https://gist.github.com/joedevivo/1f5448cc73f23cd9619c
REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

compile: $(REBAR3)
	@$(REBAR3) compile

rel: all
	@$(REBAR3) release

clean:
	@$(REBAR3) clean

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3
