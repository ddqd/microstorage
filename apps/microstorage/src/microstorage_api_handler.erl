-module(microstorage_api_handler).

-export([
    init/3
  ]).

-behaviour(cowboy_resource_handler).

-export([
    allowed/2,
    authorize/3,
    call/3,
    delete/2,
    get/2,
    patch/3,
    post/3,
    put/3
  ]).

init(_Transport, _Req, _Options) ->
  {upgrade, protocol, cowboy_resource}.

authorize(_Type, _Credentials, _Options) ->
  {ok, {<<"">>, <<"">>}}.

allowed(_, none) ->
  false;
allowed(_Method, {_Identity, _Scope}) ->
  true.

get(Query, _Options) ->
  {ok, [<<"get">>]}.

post(Entity, Query, _Options) ->
{ok, [<<"post">>]}.

put(_Entity, _Query, _Options) ->
  {ok, []}.

patch(_Changes, _Query, _Options) ->
  {ok, []}.

delete(_Query, _Options) ->
  {ok, []}.

call(_, _, _) ->
  {ok, []}.

