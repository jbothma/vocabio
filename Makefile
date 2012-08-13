ERL=erl
REBAR=./rebar

.PHONY: get-deps compile test

all: compile

compile: get-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean

get-deps:
	@$(REBAR) get-deps

test: compile boss.test.config
	@$(REBAR) boss c=test_functional

boss.test.config:
	cp boss.config boss.test.config
