%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 10月 2021 11:47
%%%-------------------------------------------------------------------
-module(eorm_emysql_adaptor).
-author("Administrator").
-include("eorm.hrl").
-include_lib("emysql/include/emysql.hrl").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([
  execute_operations/2,
  execute_operation/2,
  start/2,
  stop/1,
  select_table_info/2
]).


-export([
  test_select/0
]).

%%%============================================================
%%% API
%%%============================================================

%%------------------------------------------
%% @doc
%% 启动DB实例
%% @end
%%------------------------------------------
-type db_args() ::
  {user, string()} |
  {password, string()} |
  {host, string()} |
  {pool_size, integer()} |
  {port, integer()} |
  {database, string()} |
  {encoding, utf8}.
-spec start(atom(), db_args()) -> ok.
start(Ref, Args) when is_atom(Ref) ->
  User = proplists:get_value(user, Args),
  Password = proplists:get_value(password, Args),
  Host = proplists:get_value(host, Args),
  PoolSize = proplists:get_value(pool_size, Args),
  Port = proplists:get_value(port, Args),
  Database = proplists:get_value(database, Args),
  Encoding = proplists:get_value(encoding, Args),
  ok = emysql:add_pool(Ref, PoolSize, User, Password, Host, Port, Database, Encoding).

-spec stop(Ref::atom()) ->ok.
stop(Ref) ->
  emysql:remove_pool(Ref).

-spec select_table_info(Ref::atom(), Table::atom()) -> ok.
select_table_info(Ref, Table) ->
  Sql = <<"desc ", (atom_to_binary(Table, utf8))/binary, ";">>,
  #result_packet{
    field_list = Fields,
    rows = Rows
  } = emysql:execute(Ref, Sql),
  %% Sort Fields
  Fields2 = lists:sort(
    fun(Field1, Field2) ->
      Field1#field.seq_num < Field2#field.seq_num
    end,
    Fields
  ),

  Fields3 = lists:map(
    fun(Field) ->
      Field#field.name
    end,
    Fields2
  ),

  TableMeta = #eorm_table_meta{
    table_name = Table,
    fields = [],
    primary_key = undefined
  },

  lists:foldl(
    fun(Row, TableMeta) ->
      FieldInfo = maps:from_list(lists:zip(Fields3, Row)),
      FieldMeta = #eorm_field_meta{
        name = binary_to_atom_(maps:get(<<"Field">>, FieldInfo)),
        type = parse_type(maps:get(<<"Type">>, FieldInfo), maps:get(<<"Default">>, FieldInfo))
      },
      TableMeta1 = TableMeta#eorm_table_meta{
        fields = [FieldMeta|TableMeta#eorm_table_meta.fields]
      },
      IsPrimary = maps:get(<<"Key">>, FieldInfo) =:= <<"PRI">>,
      if
        IsPrimary ->
          IsAuto = binary:match(maps:get(<<"Extra">>, FieldInfo, <<>>), <<"auto_increment">>) =/= nomatch,
          Primary = #eorm_primary_key{
            field = FieldMeta#eorm_field_meta.name,
            auto_increment = IsAuto
          },
          TableMeta1#eorm_table_meta{ primary_key = Primary };
        true ->
          TableMeta1
      end
    end,
    TableMeta,
    Rows
  ).

%%-----------------------------------------
%% @doc
%% @end
%%------------------------------------------
execute_operations(DBRef, Operations) ->
  lists:map(
    fun(Operation) ->
      execute_operation(DBRef, Operation)
    end,
    Operations
  ).

%%%============================================================
%%% Internal Function
%%%============================================================

execute_operation(DBRef, #eorm_update_operation{} = UpdateOperation) ->
  #eorm_update_operation{
    table_name = TableName,
    key_name = KeyName,
    key_value = KeyValue,
    changes = Changes
  } = UpdateOperation,
  ?assert(Changes =/= []),
  Packet = update(DBRef, TableName, Changes, {'=', KeyName, KeyValue}),
  convert_packet(Packet);
execute_operation(DBRef, #eorm_insert_operation{} = InsertOperation) ->
  #eorm_insert_operation{
    table_name = TableName,
    fields_name = FieldsName,
    records = Records
  } = InsertOperation,
  Packet = insert(DBRef, TableName, FieldsName, Records),
  convert_packet(Packet);
execute_operation(DBRef, #eorm_select_operation{} = SelectOperation) ->
  #eorm_select_operation{
    table_name = TableName,
    key_value = KeyValue,
    key_name = KeyName
  } = SelectOperation,
  Packet = select(DBRef, TableName, all, {'=', KeyName, KeyValue}),
  convert_packet(Packet);
execute_operation(DBRef, #eorm_delete_operation{} = DeleteOperation) ->
  #eorm_delete_operation{
    table_name = TableName,
    key_name = KeyName,
    key_value = KeyValue
  } = DeleteOperation,
  Packet = delete(DBRef, TableName, {'=', KeyName, KeyValue}),
  convert_packet(Packet).

%%--------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------
select(DBRef, TableName, Fields, Conditions) ->
  {Statement, Values} = make_select_statement(TableName, Fields, Conditions),
  emysql:execute(DBRef, Statement, Values).


%%--------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------
insert(DBRef, TableName, Fields, Rows) ->
  Statement = make_insert_statement(TableName, Fields, length(Rows)),
  emysql:execute(DBRef, Statement, lists:flatten(Rows)).

%%---------------------------------------------------------
%% @doc
%%  Replace Statement
%% @end
%%---------------------------------------------------------
replace(DBRef, TableName, Fields, Rows) ->
  Statement = make_replace_statement(TableName, Fields, length(Rows)),
  emysql:execute(DBRef, Statement, lists:flatten(Rows)).


%%--------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------
delete(DBRef, TableName, Conditions) ->
  {Statement, Values} = make_delete_statement(TableName, Conditions),
  emysql:execute(DBRef, Statement, Values).




%%--------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------
update(DBRef, TableName, UpdateList, Conditions) ->
  {Statement, Values} = make_update_statement(TableName, UpdateList, Conditions),
  emysql:execute(DBRef, Statement, Values).


make_select_statement(TableName, Fields, Conditions) ->
  TableNameBin = key_to_bin(TableName),
  FieldsBin = case Fields of
    all ->
      <<"*">>;
    _ ->
      binary_join(lists:map( fun key_to_bin/1, Fields), <<",">>)
  end,
  {ConditionsBin, ConditionsValue} = make_conditions_statement(Conditions),
  Sql = <<
    "SELECT ",
    FieldsBin/binary,
    " FROM ",
    TableNameBin/binary,
    " WHERE ",
    ConditionsBin/binary,
    ";"
  >>,
  {Sql, ConditionsValue}.


make_update_statement(TableName, UpdateList, Conditions) ->
  TableNameBin = key_to_bin(TableName),
  {Fields, SettingValues} = lists:unzip(UpdateList),
  FieldsBin = lists:map(
    fun(Field) ->
      FieldBin = key_to_bin(Field),
      <<FieldBin/binary, "=?">>
    end,
    Fields
  ),
  SettingPart = binary_join(FieldsBin, <<",">>),
  {ConditionsBin, ConditionsValue} = make_conditions_statement(Conditions),
  Sql = <<
    "UPDATE",
    " ",
    TableNameBin/binary,
    " SET ",
    SettingPart/binary,
    " WHERE ",
    ConditionsBin/binary,
    ";"
  >>,
  AllValues = SettingValues ++ ConditionsValue,
  {Sql, AllValues}.


%% ---------------------------------------------------
%% @doc
%%
%% @end
%% ---------------------------------------------------
make_delete_statement(TableName, Conditions) ->
  TableNameBin = key_to_bin(TableName),
  {ConditionsBin, ConditionsValue} = make_conditions_statement(Conditions),
  Sql = <<
    "DELETE FROM ",
    TableNameBin/binary,
    " WHERE ",
    ConditionsBin/binary,
    ";"
  >>,
  {Sql, ConditionsValue}.

make_insert_statement(TableName, Fields, RowCount) ->
  TableNameBin    =   key_to_bin(TableName),
  FieldsBin = lists:map(
    fun(Field) ->
      key_to_bin(Field)
    end,
    Fields
  ),
  FieldsPart = binary_join(FieldsBin, <<",">>),
  L = [<<"?">> || _ <- lists:seq(1, length(Fields))],
  Query = <<"(", (binary_join(L, <<",">>))/binary, ")">>,
  QueryL = [Query || _ <- lists:seq(1, RowCount)],
  QueryPart = binary_join(QueryL, <<",">>),
  <<
    "INSERT INTO ",
    TableNameBin/binary,
    "(", FieldsPart/binary, ") VALUES ",
    QueryPart/binary,
    ";"
  >>.

make_replace_statement(TableName, Fields, RowCount) ->
  TableNameBin    =   key_to_bin(TableName),
  FieldsBin = lists:map(
    fun(Field) ->
      key_to_bin(Field)
    end,
    Fields
  ),
  FieldsPart = binary_join(FieldsBin, <<",">>),
  L = [<<"?">> || _ <- lists:seq(1, length(Fields))],
  Query = <<"(", (binary_join(L, <<",">>))/binary, ")">>,
  QueryL = [Query || _ <- lists:seq(1, RowCount)],
  QueryPart = binary_join(QueryL, <<",">>),
  <<
    "REPLACE INTO ",
    TableNameBin/binary,
    "(", FieldsPart/binary, ") VALUES ",
    QueryPart/binary,
    ";"
  >>.


make_conditions_statement({AndOr, Conditions}) when is_list(Conditions), length(Conditions) > 0, (AndOr =:= 'and' orelse AndOr == 'or' )->
  MapResult	=	lists:map(fun make_conditions_statement/1, Conditions),
  {Statements, Values}	=	lists:unzip(MapResult),
  Connector	=	erlang:atom_to_binary(AndOr, utf8),
  Statement	=	binary_join(Statements, <<" ", Connector/binary, " ">>),
  ValueList	=	lists:flatten(Values),
  {<<$(, Statement/binary, $)>>, ValueList};
make_conditions_statement({Operation, Key, Value})->
  OperationBin	=	convert_operation(Operation),
  KeyBin	=	key_to_bin(Key),
  Statement = <<
    $(,
    KeyBin/binary,
    " ",
    OperationBin/binary,
    " ? ",
    $)
  >>,
  {Statement, [Value]}.

convert_operation(Operation) when is_atom(Operation) ->
  erlang:atom_to_binary(Operation, utf8);
convert_operation(Operation) when is_list(Operation) ->
  erlang:list_to_binary(Operation);
convert_operation(Operation) when is_binary(Operation) ->
  Operation.


key_to_bin(Key) when is_atom(Key) ->
  <<"`", (erlang:atom_to_binary(Key, utf8))/binary, "`">>.


binary_join([], _S) -> <<>>;
binary_join(BinaryList, S) ->
  binary_join(BinaryList, S, <<>>).

binary_join([Bin], _S, Acc) ->
  <<Acc/binary, Bin/binary>>;
binary_join([Bin|T], S, Acc) ->
  binary_join(T, S, <<Acc/binary, Bin/binary, S/binary>>).


convert_packet(#ok_packet{} = Ok)  ->
  #eorm_ok_packet{
    last_insert_id = Ok#ok_packet.insert_id,
    affected_rows = Ok#ok_packet.affected_rows,
    warnings = Ok#ok_packet.warning_count,
    info = Ok#ok_packet.msg
  };
convert_packet(#result_packet{} = Result)  ->
  #eorm_result_packet{
    field_list = lists:map(fun(Field) -> binary_to_atom_(Field#field.name) end, Result#result_packet.field_list),
    rows = Result#result_packet.rows,
    extra = Result#result_packet.extra
  };
convert_packet(#error_packet{} = Err) ->
  #eorm_err_packet{
    error_code = Err#error_packet.code,
    error_message = Err#error_packet.msg
  }.


binary_to_atom_(Bin) ->
  case catch binary_to_existing_atom(Bin, utf8) of
    {'EXIT', _} ->
      binary_to_atom(Bin, utf8);
    Atom ->
      Atom
  end.

parse_type(<<"char", Len/binary>>, Default) ->
  {char, parse_len(Len), Default};
parse_type(<<"varchar", Max/binary>>, Default) ->
  {varcahr, parse_len(Max), Default};

parse_type(<<"int", Extra/binary>>, DefaultBin) ->
  Default = parse_default(DefaultBin),
  case binary:match(<<"unsigned">>, Extra) of
    nomatch ->
      {int32, parse_len(Extra), Default};
    _ ->
      {uint32, parse_len(Extra), Default}
  end;

parse_type(<<"smallint", Extra/binary>>, DefaultBin) ->
  Default = parse_default(DefaultBin),
  case binary:match(<<"unsigned">>, Extra) of
    nomatch ->
      {int16, parse_len(Extra), Default};
    _ ->
      {uint16, parse_len(Extra), Default}
  end;

parse_type(<<"bigint", Extra/binary>>, DefaultBin) ->
  Default = parse_default(DefaultBin),
  case binary:match(<<"unsigned">>, Extra) of
    nomatch ->
      {int64, parse_len(Extra), Default};
    _ ->
      {uint64, parse_len(Extra), Default}
  end;

parse_type(<<"tinyint", Extra/binary>>, DefaultBin) ->
  Default = parse_default(DefaultBin),
  case binary:match(<<"unsigned">>, Extra) of
    nomatch ->
      {int8, parse_len(Extra), Default};
    _ ->
      {uint8, parse_len(Extra), Default}
  end;

parse_type(<<"text", _/binary>>, Default) ->
  {text, Default};

parse_type(<<"blob", _/binary>>, Default) ->
  {blob, Default}.

parse_len(Bin) ->
  {S, _} = binary:match(Bin, <<"(">>),
  {E, _} = binary:match(Bin, <<")">>),
  binary_to_integer(binary:part(Bin, S + 1, E - S - 1)).

parse_default(undefined) ->
  undefined;
parse_default(Bin) ->
  binary_to_integer(Bin).

%%%=================================================================
%%% Testing Functions
%%%=================================================================

test_select() ->
  start(
    ?MODULE,
    [
      {user, "root"},
      {password, "root"},
      {host, "127.0.0.1"},
      {pool_size, 8},
      {port, 3306},
      {database, "eorm_test"},
      {encoding, utf8}
    ]
  ),
  execute_operation(
    ?MODULE,
    #eorm_select_operation{
      table_name = eorm_demo,
      key_name = guid,
      key_value = 1001
    }
  ),
  select_table_info(?MODULE, eorm_demo).
