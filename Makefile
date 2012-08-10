ERL=erl
REBAR=./rebar

.PHONY: deps get-deps

all: compile

compile: get-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

test:
	@$(REBAR) boss c=test_functional
