all:
	(rebar get-deps compile generate)

clean:
	(rebar clean)

distclean:
	(rebar clean delete-deps)

tests:
	(rebar eunit)

docs:
	(rebar doc)

build_plt:
	(dialyzer --build_plt --output_plt dialyzer.plt --apps erts kernel stdlib crypto mnesia sasl common_test ssl reltool eunit -r ./deps/*/ebin)

check:
	(dialyzer --plt dialyzer.plt -I ./include/ -c ./src/*.erl)
