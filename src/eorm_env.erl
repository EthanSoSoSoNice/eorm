%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 10月 2021 17:16
%%%-------------------------------------------------------------------
-module(eorm_env).
-author("Administrator").
-include("eorm.hrl").

%% API
-export([
  start_link/0,
  save/3,
  lookup/2,
  clear/0
]).


%% Behaviour
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).



%%========================================================
%% API
%%========================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec save(atom(), any(), any()) -> ok.
save(Ets, Key, Value) ->
  ets:insert(Ets, {Key, Value}),
  ok.

-spec lookup(atom(), any()) -> any() | undefined.
lookup(Ets, Key) ->
  case ets:lookup(Ets, Key) of
    [] ->
      undefined;
    [{_K, V}] ->
      V
  end.

clear() ->
  ets:delete_all_objects(?EORM_ENV_TAB),
  ets:delete_all_objects(?EORM_TABLE_META),
  ets:delete_all_objects(?EORM_TABLE_FIELDS).

%%========================================================
%% GenServer Callback
%%========================================================

init(_Args) ->
  ets:new(?EORM_ENV_TAB, [named_table, public]),
  ets:new(?EORM_TABLE_META, [named_table, public]),
  ets:new(?EORM_TABLE_FIELDS, [named_table, public]),
  {ok, undefined}.


handle_call(_Msg, _From, State) ->
  {noreply, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_Old, State, _Extra) ->
  State.

terminate(_Reason, _State) ->
  ok.
