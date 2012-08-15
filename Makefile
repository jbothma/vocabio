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

test: compile boss_test eunit ct

eunit:
	@$(REBAR) skip_deps=true eunit

boss_test: boss.test.config
	@$(REBAR) boss c=test_functional

ct:
	@$(REBAR) skip_deps=true ct

boss.test.config:
	cp boss.config boss.test.config
