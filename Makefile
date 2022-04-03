all: compile

clean:
	@rebar3 clean

nuke: clean
	@rm -rf deps

check: compile
	@rebar3 eunit skip_deps=true

dependencies:
	@rebar3 get-deps

compile: dependencies
	@rebar3 compile
