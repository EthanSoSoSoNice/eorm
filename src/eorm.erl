%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 10月 2021 17:11
%%%-------------------------------------------------------------------
-module(eorm).
-author("Administrator").
-include("eorm.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
  run/2,
  stop/1,
  load_object/3,
  load_table/2,
  register_table/2
]).


%%-------------------------------------------
%% API
%%-------------------------------------------

%% @doc
%% params [
%%    {db_args, eorm_database:db_args()}
%%    {auto_loading, boolean()}       default false
%%    {adaptor, emysql}               default emysql
%% ]
%% @end
-spec run(Ref::atom(), Params::list()) -> ok.
run(Ref, Params) when is_atom(Ref), is_list(Params) ->
  %% check if res is registered
  ?assert(eorm_env:lookup(?EORM_ENV_TAB, Ref) =:= undefined),

  %% check params
  DBArgs = proplists:get_value(db_args, Params, undefined),
  ?assert(DBArgs =/= undefined),
  %% todo ?assert(checkDBArgs(DBArgs)),

  %% start db pool
  Adaptor = proplists:get_value(adaptor, Params, emysql),
  Result = eorm_database:start(Ref, Adaptor, DBArgs),
  ?assertMatch(ok, Result),

  %% save env
  eorm_env:save(?EORM_ENV_TAB, {Ref, adaptor}, Adaptor),
  eorm_env:save(?EORM_ENV_TAB, Ref, true),
  ok.

%% @doc
%%
%% @end
-spec stop(Ref::atom()) -> ok.
stop(Ref) ->
  Adaptor = eorm_env:lookup(?EORM_ENV_TAB, {Ref, adaptor}),
  eorm_database:stop(Ref, Adaptor),
  eorm_env:clear().

-spec load_object(atom(), atom(), any()) -> eorm_object:eorm_object().
load_object(Ref, Type, KeyValue) ->
  TableMeta = eorm_env:lookup_table_meta(?EORM_TABLE_META, {Ref, Type}),
  ?assert(TableMeta =/= undefined),
  eorm_object:load(Ref, Type, KeyValue).

-spec load_table(atom(), atom()) -> #eorm_table_meta{}.
load_table(Ref, Tab) ->
  TableMeta = eorm_database:load_table(Ref, Tab),
  register_table(Ref, TableMeta),
  TableMeta.

%%-------------------------------------------
%%
%%-------------------------------------------
-spec register_table(Ref::atom(), TableMeta::#eorm_table_meta{}) -> ok.
register_table(Ref, TableMeta) ->
  TableName = TableMeta#eorm_table_meta.table_name,
  ?assert(eorm_env:lookup(?EORM_TABLE_META, TableName) =:= undefined),
  eorm_env:save(?EORM_TABLE_META, {Ref, TableName}, TableMeta),
  lists:foreach(
    fun(FieldMeta) ->
      FieldName = FieldMeta#eorm_field_meta.name,
      eorm_env:save(?EORM_TABLE_FIELDS, {Ref, TableName, FieldName}, FieldMeta)
    end,
    TableMeta#eorm_table_meta.fields).

%%%========================================================
%%% Internal Functions
%%%========================================================

%%%========================================================
%%% eunit test
%%%========================================================



-ifdef(EUNIT).



et_op(Ref) ->
  Demo = eorm_object:new(Ref, eorm_demo),
  Demo1 = eorm_object:set(guid, 0, Demo),
  Demo2 = eorm_object:set(name, <<"John">>, Demo1),
  Demo3 = eorm_object:flush(Demo2),
  Demo4 = eorm_object:set(age, 20, Demo3),
  Demo5 = eorm_object:flush(Demo4),
  Demo6 = eorm_object:load(Ref, eorm_demo, eorm_object:get(guid, Demo5)),
  Demo6Guid = eorm_object:get(guid, Demo6),
  Demo6Name = eorm_object:get(name, Demo6),
  eorm_object:flush(eorm_object:delete(Demo5)),
  [
    ?_assertMatch(Demo6Guid, eorm_object:get(guid, Demo5)),
    ?_assertMatch(Demo6Name, eorm_object:get(name, Demo5)),
    ?_assertMatch(undefined, eorm_object:get(guid, Demo)),
    ?_assertMatch(0, eorm_object:get(guid, Demo1)),
    %% test default value
    ?_assertMatch(<<"DefaultName">>, eorm_object:get(name, Demo1)),
    ?_assertMatch(18, eorm_object:get(age, Demo3)),
    %% test setting
    ?_assertMatch(<<"John">>, eorm_object:get(name, Demo2)),
    ?_assert(eorm_object:get(guid, Demo3) =/= 0),
    ?_assertMatch(20, eorm_object:get(age, Demo4))
  ].

case_test_() ->
  {
    setup,
    fun eorm_eunit:et_env/0,
    fun(Ref) ->
      stop(Ref)
    end,
    fun(Ref) ->
        et_op(Ref)
    end
  }.
-endif.
