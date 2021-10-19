%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 12月 2019 11:13
%%%-------------------------------------------------------------------
-module(eorm_database).
-author("Administrator").
-include("eorm.hrl").

%% API
-export([
  execute_operations/2,
  execute_operation/2,
  load_table/2,
  start/2,
  stop/1
]).

%%%============================================================
%%% API
%%%============================================================
start(Ref, Args) ->
  case eorm_env:lookup_env(Ref, adaptor) of
    emysql ->
      eorm_emysql_adaptor:start(Ref, Args)
  end.

stop(Ref) ->
  case eorm_env:lookup_env(Ref, adaptor) of
    emysql ->
      eorm_emysql_adaptor:stop(Ref)
  end.

load_table(Ref, Table) ->
  case eorm_env:lookup_env(Ref, adaptor) of
    emysql ->
      eorm_emysql_adaptor:select_table_info(Ref, Table)
  end.


%%------------------------------------------
%% @doc
%% 执行一系列的数据库操作
%% @end
%%------------------------------------------
execute_operations(DBRef, Operations) ->
  DBAdaptor = get_db_adaptor(DBRef),
  DBAdaptor:execute_operations(DBRef, Operations).

execute_operation(DBRef, Operation) ->
  DBAdaptor = get_db_adaptor(DBRef),
  DBAdaptor:execute_operation(DBRef, Operation).



%%%============================================================
%%% Internal Functions
%%%============================================================

get_db_adaptor(_DBRef) ->
  eorm_emysql_adaptor.
