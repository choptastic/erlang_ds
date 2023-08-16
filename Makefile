all:
	rebar3 compile

test:
	rebar3 eunit

dialyzer:
	rebar3 dialyzer

publish:
	rebar3 hex publish

shell:
	rebar3 shell
