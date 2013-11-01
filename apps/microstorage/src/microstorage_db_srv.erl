-module(microstorage_db_srv).
 
-behaviour(gen_server).
 
-export([start_link/0]).

-export([install/0]).
 
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
 
-record(state, {}).

-record(storage, {name, content=[]}).

-record(store, {uuid, storage=#storage{}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
 
init([]) ->
    {ok, #state{}}.
 
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
    case mnesia:create_table(store,
                [{attributes, record_info(fields, store)},
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
