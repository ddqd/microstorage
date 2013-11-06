-module(microstorage_db).
 
-export([install/0, set_data/3, get_data/2, delete_data/2]).

-include_lib("stdlib/include/qlc.hrl").

-include("microstorage.hrl").
 
%% Internal functions

create_table() ->
    case mnesia:create_table(storage,
                [{attributes, record_info(fields, storage)},
                {index, []},
                {storage_properties,[   
                    {ets, [compressed]}, 
                    {dets, [{auto_save, 3000}]} 
                ]}, {disc_copies, [node()]}]) of
        {aborted, {already_exists, Key}} -> lager:log(info, self(), "Table ~p Already Exist", [Key]);
        {atomic,ok} -> lager:log(info, self(), "Table Created");
        {aborted, Reason} -> lager:log(error, self(), "Create table Error ~p", [Reason])
    end,
    ok.

install() ->
mnesia:stop(),
mnesia_schema:delete_schema([node()]),
case  mnesia:create_schema([node()]) of 
    ok ->
        mnesia:start(),
        create_table();
    Error -> lager:log(info, self(), "Create schema error ~p", [Error])
end,
ok.

get_data(Uuid, Key) ->
    F = fun() ->   
        Query = qlc:q([ Storage || Storage <- mnesia:table(storage), ((Storage#storage.uuid == Uuid) and (Storage#storage.key == Key))] ),
        Res = qlc:e(Query),
        case Res of 
            [] -> 
                {error, not_found};
            [Storage|_] -> 
                {ok, Storage}
        end
    end,
    transaction(F).

set_data(Uuid, Key, Data) ->
    F = fun() ->
        case get_data(Uuid, Key) of
            {error, not_found} -> 
                case mnesia:write(#storage{uuid = Uuid, key  = Key, data = Data}) of 
                    ok -> {ok, [{<<"status">>, <<"ok">>}]};
                    _ -> {error, write_error}
                end;
            _ -> 
                {error, already_exists}
        end
    end,
    transaction(F).

delete_data(Uuid, Key) ->
    F = fun() ->
        case get_data(Uuid, Key) of
            {ok, Storage} ->
                case mnesia:delete_object(Storage) of 
                    ok -> {ok, [{<<"status">>, <<"ok">>}]};
                    _ -> {error, write_error}
                end;
            {error, _} ->
                {error, not_found}
        end
    end,
    transaction(F).

transaction(F) ->
    mnesia:activity(transaction, F).

storage_to_binary(Storage) ->
  Uuid = Storage#storage.uuid,
  Key = Storage#storage.key,
  Data = Storage#storage.data,
  [{<<"uuid">>, Uuid}, {<<"key">>, Key}, {<<"data">>, Data}].
