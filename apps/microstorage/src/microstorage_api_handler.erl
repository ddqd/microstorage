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

check_path(Query) ->
  [Head|Tail] = Query,
  case Head of
  {path,<<"api">>} ->
    {ok, Tail};
    _ -> {error, wrong_path}
  end.

probe_query(Query) ->
  case Query of 
  [{<<"data">>, Data},{<<"key">>,Key},{<<"method">>,<<"SET">>},{<<"uuid">>,Uuid}] ->
    microstorage_db:set_data(Uuid, Key, jsx:decode(Data));
  [{<<"key">>,Key},{<<"method">>,Method},{<<"uuid">>,Uuid}] ->
    case Method of
      <<"GET">> ->
        case microstorage_db:get_data(Uuid, Key) of 
          {ok, Storage} -> {ok, storage_to_binary(Storage)};
          Error -> Error
        end;
      <<"DELETE">> ->
        microstorage_db:delete_data(Uuid, Key);
    _ -> 
      {error, unknown_method}
    end;
  _ -> 
    {error, wrong_query}
  end.

storage_to_binary(Storage) ->
  Uuid = Storage#storage.uuid,
  Key = Storage#storage.key,
  Data = Storage#storage.data,
  [{<<"uuid">>, Uuid}, {<<"key">>, Key}, {<<"data">>, Data}].

get(Query, _Options) ->
  case check_path(Query) of 
    {ok, Q} ->
      probe_query(Q);
    Error -> Error
  end.

post(Entity, Query, _Options) ->
  case Query of
    [{path, <<"api">>}] ->
      case Entity of 
      [{<<"uuid">>,Uuid},{<<"key">>,Key},{<<"data">>, Data}] ->
        microstorage_db:set_data(Uuid, Key, Data);
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
  [{path,<<"api">>},{<<"key">>, Key},{<<"uuid">>,Uuid}] ->
    microstorage_db:delete_data(Uuid, Key);
  _ -> {error, format_error}
end.

call(_, _, _) ->
  {ok, []}.
