-module(microstorage_ttl_srv).

-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1]).

-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

 -define(DEFAULT_UPDATE_TTL_SEC, 60).
 -define(WITH_DEFAULT(X, Y), case X of
                                undefined ->
                                    {ok, Y};
                                _ ->
                                    X
                            end).

get_upd_ttl_sec() ->
    {ok, TTL} = ?WITH_DEFAULT(application:get_env(microstorage, upd_ttl_sec), ?DEFAULT_UPDATE_TTL_SEC),
    TTL.

-record(state, {timer}).

init([]) ->
  Timer = erlang:send_after(1, self(), tick),
  State = #state{timer = Timer},
  {ok, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(tick, State) ->
  erlang:cancel_timer(State#state.timer),
  microstorage_db:upd(get_upd_ttl_sec()),
  Timer = erlang:send_after(get_upd_ttl_sec()*1000, self(), tick),
  NewState = State#state{timer = Timer},
  {noreply, NewState};

handle_info(_Info, State) ->
  {noreply, State}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call({local, ?MODULE}, stop).

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


