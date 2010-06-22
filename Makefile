.PHONY: rel deps

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

rel: deps
	@./rebar compile generate

relforce: deps
	@./rebar compile generate force=1

clean:
	@./rebar clean

distclean: clean relclean
	@./rebar delete-deps

relclean:
	rm -rf rel/wriaki
