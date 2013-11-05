-module(microstorage_db_srv).
 
-behaviour(gen_server).
 
-export([start_link/0]).

-export([install/0]).

-export([get_data/2, store_data/3, remove_data/2]).
 
-include_lib("stdlib/include/qlc.hrl").

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(storage, {uuid, name=[], data=[]}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    {ok}.
 
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%% Internal functions

create_table() ->
    case mnesia:create_table(storage,
                [{attributes, record_info(fields, storage)},
                {index, []},
                {storage_properties,[   
                    {ets, [compressed]}, 
                    {dets, [{auto_save, 3000}]} 
                ]}, {disc_copies, [node()]}]) of
        {aborted, {already_exists, Name}} -> lager:log(info, self(), "Table ~p Already Exist", [Name]);
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

get_data(Uuid, Name) ->
    F = fun() ->   
        Query = qlc:q([ Storage || Storage <- mnesia:table(storage), ((Storage#storage.uuid == Uuid) and (Storage#storage.name == Name))] ),
        Res = qlc:e(Query),
        case Res of 
            [] -> 
                {error, not_found};
            [Storage|_] -> 
                {ok, Storage}
        end
    end,
    transaction(F).

store_data(Uuid, Name, Data) ->
    F = fun() ->
        case get_data(Uuid, Name) of
            {error, not_found} -> mnesia:write(#storage{uuid = Uuid, name  = Name, data = Data});
            _ -> {error, already_exists}
        end
    end,
    transaction(F).

remove_data(Uuid, Name) ->
    F = fun() ->
        case get_data(Uuid, Name) of
            {ok, Storage} ->
                mnesia:delete_object(Storage);
            {error, _} ->
                {error, not_found}
        end
    end,
    transaction(F).

transaction(F) ->
    mnesia:activity(transaction, F).
