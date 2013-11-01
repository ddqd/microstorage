-module(microstorage_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Config = {port, 8081},
	{_, Port} = Config, 
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", microstorage_api_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [Config], [
		{env, [{dispatch, Dispatch}]}
	]),
	lager:log(info, self(), "microstorage_api started on ~p:~p", [node(), Port]),
	microstorage_sup:start_link().

stop(_State) ->
    ok.