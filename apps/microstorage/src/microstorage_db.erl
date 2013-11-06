-module(microstorage_db).
 
-export([install/0, set_data/3, get_data/2, delete_data/2, get_all/0, upd/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("microstorage.hrl").
 
%% Internal functions

 -define(DEFAULT_TTL_SEC, 666).
 -define(WITH_DEFAULT(X, Y), case X of
                                undefined ->
                                    {ok, Y};
                                _ ->
                                    X
                            end).

get_ttl() ->
    {ok, TTL} = ?WITH_DEFAULT(application:get_env(microstorage, ttl_sec), ?DEFAULT_TTL_SEC),
    TTL.

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
                reset_ttl(Storage), 
                {ok, Storage}
        end
    end,
    transaction(F).

set_data(Uuid, Key, Data) ->
    F = fun() ->
        case get_data(Uuid, Key) of
            {error, not_found} -> 
                case mnesia:write(#storage{uuid = Uuid, key  = Key, data = Data, ttl=get_ttl()}) of 
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

upd_ttl(Storage, Time) when Storage#storage.ttl > 0 ->
        mnesia:write(Storage#storage{ttl = Storage#storage.ttl - Time});

upd_ttl(Storage, _) when Storage#storage.ttl =< 0 ->
        mnesia:delete_object(Storage).

reset_ttl(Storage) ->
    F = fun() ->
        mnesia:write(Storage#storage{ttl=get_ttl()})   
    end,
    transaction(F).

upd(Time) ->
    F = fun() ->
            Q = qlc:q(
                [upd_ttl(Storage, Time) || Storage<- mnesia:table(storage)]
            ), 
            qlc:e(Q)
    end,
    transaction(F).

get_all() ->
    mnesia:transaction( 
        fun() ->
            qlc:eval( qlc:q(
                [ X || X <- mnesia:table(storage)] 
            )) 
    end ).

transaction(F) ->
    mnesia:activity(transaction, F).


