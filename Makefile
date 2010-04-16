.PHONY: rel

all:
	@./rebar compile

rel: 
	@./rebar compile generate

relforce: 
	@./rebar compile generate force=1

clean:
	@./rebar clean

