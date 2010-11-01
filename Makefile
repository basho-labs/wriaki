export ICU_CFLAGS=$(shell icu-config --cppflags-searchpath) \
                  $(shell icu-config --cflags)
export ICU_LDFLAGS=$(shell icu-config --ldflags) \
                   $(shell icu-config --ldflags-icuio)

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

stage : rel
	cd rel/wriaki/lib && \
	rm -rf wiki_creole-* wriaki-* && \
	ln -s ../../../apps/wiki_creole && \
	ln -s ../../../apps/wriaki

test:
	./rebar skip_deps=true eunit
