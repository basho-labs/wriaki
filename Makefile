.PHONY: rel

all:
	@./rebar compile

rel: all
	@./rebar generate

relforce: all
	@./rebar generate force=1
