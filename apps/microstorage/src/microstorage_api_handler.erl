-module(microstorage_api_handler).

-export([
    init/3
  ]).

-behaviour(cowboy_resource_handler).

-include("microstorage.hrl").

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
  true;
allowed(_Method, {_Identity, _Scope}) ->
  true.

get(Query, _Options) ->
  case Query of
    [{path,<<"api">>},{<<"name">>, Name},{<<"uuid">>, Uuid}] ->
      case gen_server:call(microstorage_db_srv, {get, Uuid, Name}) of 
        {ok, Storage} ->
          {ok, [{<<"status">>, <<"ok">>}]++storage(Storage)};
        Error ->
          Error
      end;
    _ -> {error, wrong_path} 
  end.

post(Entity, Query, _Options) ->
  case Query of
    [{path, <<"api">>}] ->
      case Entity of 
      [{<<"uuid">>,Uuid},{<<"name">>,Name},{<<"data">>, Data}] ->
        Reply = gen_server:call(microstorage_db_srv, {store, Uuid, Name, Data}),
        case Reply of 
          ok -> {ok, [{<<"status">>, <<"ok">>}]};
          _ ->
            Reply          
        end;
      _ ->
        {error, wrong_json}
    end;
    _ -> 
      {error, wrong_path}
  end.

put(_Entity, _Query, _Options) ->
  {ok, <<"put">>}.

patch(_Changes, _Query, _Options) ->
  {ok, []}.

delete(Query, _Options) ->
  case Query of
  [{path,<<"api">>},{<<"name">>, Name},{<<"uuid">>,Uuid}] ->
    case gen_server:call(microstorage_db_srv, {delete, Uuid, Name}) of 
      ok -> {ok, [{<<"status">>, <<"ok">>}]};
      Error -> Error
    end;
  _ -> {error, format_error}
end.

call(_, _, _) ->
  {ok, []}.

storage(Storage) ->
  Uuid = Storage#storage.uuid,
  Name = Storage#storage.name,
  Data = Storage#storage.data,
  [{<<"uuid">>, Uuid}, {<<"name">>, Name}, {<<"data">>, Data}].


