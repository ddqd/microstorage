NODE=microstorage
HOST=localhost
COOKIE = WOWSUCHSECRETWORD
NODENAME=$(NODE)@$(HOST)

all: reqs compile

reqs:
	rebar get-deps

compile: clean
	rebar compile

clean:
	rm -rf tmp
	rebar clean

run:
	(mkdir -p tmp)
	rebar compile
	erl -sname $(NODENAME) -setcookie $(COOKIE) -config config/microstorage.config -pa apps/*/ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel,  asn1, crypto, public_key, mimetypes, lager, ranch, inets, ssl, sync, quickrand, uuid, mnesia, emysql, cowlib, cowboy, ibrowse, microstorage] ]'

shell:
	rebar compile 	
	(mkdir -p tmp)
	erl +K true -sname $(NODENAME) -config config/$(NODE) -setcookie $(COOKIE) -pa apps/*/ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel, crypto, lager, ranch, inets, sync, uuid, poolboy, mnesia]]'

bootstrap:
	(mkdir -p tmp)
	rebar get-deps
	rebar compile
	screen -S microstorage -d -m erl -sname $(NODENAME) -setcookie $(COOKIE) -config config/microstorage.config -pa apps/*/ebin deps/*/ebin -eval '[application:start(A) || A <- [kernel,  asn1, crypto, public_key, mimetypes, lager, ranch, inets, ssl, sync, quickrand, uuid, mnesia, emysql, cowlib, cowboy, ibrowse, microstorage] ]' -s microstorage_db_srv install

stop:
	erl -sname cc@localhost -s mnesia -setcookie  $(COOKIE) -eval 'rpc:call($(NODENAME), init, stop, [])' -s init stop