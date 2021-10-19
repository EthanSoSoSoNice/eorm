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
  save_env/3,
  save_table_meta/2,
  save_field_meta/3,
  lookup_field_meta/3,
  lookup_table_meta/2,
  lookup_env/2,
  clear/1
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

-spec save_table_meta(atom(), #eorm_table_meta{}) -> ok.
save_table_meta(Ref, TableMeta)->
  save(?EORM_TABLE_META, {Ref, TableMeta#eorm_table_meta.table_name}, TableMeta).

save_field_meta(Ref, TableName, FieldMeta) ->
  save(?EORM_TABLE_FIELDS, {Ref, TableName,FieldMeta#eorm_field_meta.name}, FieldMeta).

lookup_field_meta(Ref, TableName, FieldName) ->
  lookup(?EORM_TABLE_FIELDS, {Ref, TableName, FieldName}).

save_env(Ref, Key, Value) ->
  save(?EORM_ENV_TAB, {Ref, Key}, Value).

-spec lookup_env(atom(), any()) -> any() | undefined.
lookup_env(Ref, Key) ->
  lookup(?EORM_ENV_TAB, {Ref, Key}).

-spec lookup_table_meta(atom(), atom()) -> #eorm_table_meta{} | undefined.
lookup_table_meta(Ref, Tab) ->
  lookup(?EORM_TABLE_META, {Ref, Tab}).

clear(Ref) ->
  ets:match_delete(?EORM_TABLE_META, {{Ref, '_'}, '_'}),
  ets:match_delete(?EORM_ENV_TAB, {{Ref, '_'}, '_'}),
  ets:match_delete(?EORM_TABLE_FIELDS, {{Ref, '_', '_'}, '_'}).

%%==============================================================
%% Internal
%%==============================================================

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
