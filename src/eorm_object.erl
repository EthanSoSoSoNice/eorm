%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% @end
%%% Created : 13. 10月 2021 14:10
%%%-------------------------------------------------------------------
-module(eorm_object).
-author("Administrator").
-include("eorm.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
  new/2,
  new/3,
  load/3,
  get/2,
  set/2,
  set/3,
  delete/1,
  flush/1
]).

-type eorm_object() :: maps:maps().
-type key () :: atom().
-type value() :: any().

-export_type([
  eorm_object/0
]).

%%%===========================================
%%% API
%%%===========================================
-spec new(atom(), atom()) -> eorm_object().
new(Ref, Type) ->
  O = #{
    '__meta' => create_meta(Ref, Type, true)
  },
  Fields = eorm_object_model:get_fields(Ref, Type),
  %% set default value
  lists:foldl(
    fun(Field, O1) ->
      FieldName = Field#eorm_field_meta.name,
      maps:put(FieldName, eorm_object_model:get_default_value(Ref, Type, FieldName), O1)
    end,
    O,
    Fields
  ).

%% @doc
%% [{Key, Value}...]
%% @end
-spec new(atom(), atom(), list()) -> eorm_object().
new(Ref, Type, Row) ->
  O = #{
    '__meta' => create_meta(Ref, Type, true)
  },
  Fields = eorm_object_model:get_fields(Ref, Type),
  %% set default value
  O2 = lists:foldl(
    fun(Field, O1) ->
      FieldName = Field#eorm_field_meta.name,
      maps:put(FieldName, eorm_object_model:get_default_value(Ref, Type, FieldName), O1)
    end,
    O,
    Fields
  ),
  lists:foldl(
    fun({K, V}, O1) ->
      maps:put(K, V, O1)
    end,
    O2,
    Row
  ).

-spec load(atom(), atom(), any()) -> eorm_object().
load(Ref, Type, Value) ->
  #eorm_table_meta{
    table_name = TableName,
    primary_key = #eorm_primary_key{
      field = PrimaryKey
    }
  } = eorm_env:lookup(?EORM_TABLE_META, {Ref, Type}),
  SelectOp = #eorm_select_operation{
    table_name = TableName,
    key_value = Value,
    key_name = PrimaryKey
  },
  #eorm_result_packet{
    field_list = Fields,
    rows = [Row]
  } = eorm_database:execute_operation(Ref, SelectOp),
  new(Ref, Type, lists:zip(Fields, Row)).


%% @doc
%%
%% @end
-spec set(key(), value(), eorm_object()) -> eorm_object().
set(Key, Value, Object) ->
  ?assert(Key =/= '__meta'),
  Object1 = maps:put(Key, Value, Object),
  IsNew = is_new(Object),
  if
    IsNew ->
      Object1;
    true ->
      mark_key(Object1, Key)
  end.

%% @doc
%% Properties = [{Key, Value}]
%% @end
-spec set(list(), eorm_object()) -> eorm_object().
set(Properties, Object) ->
  lists:foldl(
    fun({K, V}, O) ->
      set(K, V, O)
    end,
    Object,
    Properties
  ).

%% @doc
%% field is not marked as dirty
%% @end
-spec raw_set(key(), value(), eorm_object()) -> eorm_object().
raw_set(Key, Value, Object) ->
  %% todo check key is valid
  maps:put(Key, Value, Object).


%% @doc
%% error if there's no the key
%% @end
-spec get(key(), eorm_object()) -> value().
get(Key, Object) ->
  maps:get(Key, Object).

%% @doc
%% the object is marked as deleted.
%% call flush/1 to delete the object in database
%% @end
-spec delete(eorm_object()) -> eorm_object().
delete(Object) ->
  Meta = get_meta(Object),
  Meta1 = maps:put('__deleted', true, Meta),
  maps:put('__meta', Meta1, Object).


%% @doc
%% sync to database
%% @end
-spec flush(eorm_object()) -> eorm_object().
flush(Object) ->
  Action = switch_action(Object),
  Op = case Action of
    delete ->
      make_delete(Object);
    insert ->
      make_insert(Object);
    update ->
      make_update(Object)
  end,
  Ref = get_ref(Object),
  case eorm_database:execute_operation(Ref, Op) of
    #eorm_ok_packet{} = OkPacket ->
      post_executing_ok(Action, Ref, OkPacket, Object);
    Err ->
      %%todo
      error(Err)
  end.





%%%================================================
%%% Internal Function
%%%================================================


post_executing_ok(insert, Ref, OKPacket, Object) ->
  Primary = eorm_object_model:get_primary_key(Ref, get_type(Object)),
  AutoInc = Primary#eorm_primary_key.auto_increment,
  KeyValue = get(Primary#eorm_primary_key.field, Object),
  if
    AutoInc and (KeyValue =:= 0 orelse KeyValue =:= undefined)->
      Object1 = raw_set(Primary#eorm_primary_key.field, OKPacket#eorm_ok_packet.last_insert_id, Object),
      mark_old(Object1);
    true ->
      mark_old(Object)
  end;
post_executing_ok(update, _Ref, _OKPacket, Object) ->
  clear_dirty(Object);
post_executing_ok(_, _, _, Object) ->
  Object.

-spec switch_action(eorm_object()) -> insert | update |insert.
switch_action(Object) ->
  IsDeleted = is_deleted(Object),
  if
    IsDeleted ->
      delete;
    true ->
      IsNew = is_new(Object),
      if
        IsNew ->
          insert;
        true ->
          update
      end
  end.

%% @doc
%% mark a key
%% @end
-spec mark_key(eorm_object(), atom()) -> eorm_object().
mark_key(Object, Key) ->
  Meta = get_meta(Object),
  #{ '__dirty' := Dirty } = Meta,
  Dirty1 = maps:put(Key, true, Dirty),
  Meta1 = maps:put('__dirty', Dirty1, Meta),
  maps:put('__meta', Meta1, Object).


-spec mark_old(eorm_object()) -> eorm_object().
mark_old(Object) ->
  Meta = get_meta(Object),
  Meta1 = maps:put('__is_new', false, Meta),
  maps:put('__meta', Meta1, Object).


-spec clear_dirty(eorm_object()) -> eorm_object().
clear_dirty(Object) ->
  Meta = get_meta(Object),
  Meta1 = maps:put('__dirty', #{}, Meta),
  maps:put('__meta', Meta1, Object).

-spec create_meta(atom(), atom(), boolean()) -> maps:maps().
create_meta(Ref, Type, IsNew) ->
  #{
    '__ref' => Ref,
    '__is_new' => IsNew,
    '__dirty' => #{},
    '__type' => Type,
    '__deleted' => false
  }.


-spec get_type(eorm_object()) -> atom().
get_type(Object) ->
  maps:get('__type', get_meta(Object)).


-spec get_dirty(eorm_object()) -> maps:maps().
get_dirty(Object) ->
  maps:get('__dirty', get_meta(Object)).

-spec get_ref(eorm_object()) -> atom().
get_ref(Object) ->
  maps:get('__ref', get_meta(Object)).


-spec get_meta(eorm_object()) -> maps:maps().
get_meta(Object) ->
  maps:get('__meta', Object).


-spec is_deleted(eorm_object()) -> boolean().
is_deleted(Object) ->
  maps:get('__deleted', get_meta(Object)) =:= true.


-spec is_new(eorm_object()) -> boolean().
is_new(Object) ->
  maps:get('__is_new', get_meta(Object)) =:= true.


%%%=================================================
%%% Make Operation
%%%=================================================

-spec make_insert(eorm_object()) -> #eorm_insert_operation{}.
make_insert(Object) ->
  Type = get_type(Object),
  Ref = get_ref(Object),
  Fields = eorm_object_model:get_fields(Ref, Type),
  Pairs = lists:map(
    fun(Field) ->
      FieldName = Field#eorm_field_meta.name,
      {FieldName, get(Field#eorm_field_meta.name, Object)}
    end,
    Fields
  ),
  {F, V} = lists:unzip(Pairs),
  #eorm_insert_operation{
    table_name = get_type(Object),
    fields_name = F,
    records = [V]
  }.

-spec make_update(eorm_object()) -> #eorm_update_operation{}.
make_update(Object) ->
  Dirty = get_dirty(Object),
  Type = get_type(Object),
  Ref = get_ref(Object),
  Primary = (eorm_object_model:get_primary_key(Ref, Type))#eorm_primary_key.field,
  Value = get(Primary, Object),
  Changes = lists:map(
    fun(DirtyKey) ->
      {DirtyKey, get(DirtyKey, Object)}
    end,
    maps:keys(Dirty)
  ),
  #eorm_update_operation{
    table_name = Type,
    key_name = Primary,
    key_value = Value,
    changes = Changes
  }.


-spec make_delete(eorm_object()) -> #eorm_delete_operation{}.
make_delete(Object) ->
  Type = get_type(Object),
  Ref = get_ref(Object),
  PrimaryKey = (eorm_object_model:get_primary_key(Ref, Type))#eorm_primary_key.field,
  Value = get(PrimaryKey, Object),
  #eorm_delete_operation{
    table_name = Type,
    key_name = PrimaryKey,
    key_value = Value
  }.


%%%=======================================================
%%% Testing Functions
%%%=======================================================

-ifdef(EUNIT).

base_test_() ->
  {
    setup,
    fun eorm_eunit:et_env/0,
    fun(Ref) -> eorm:stop(Ref) end,
    fun(Ref) ->
      Test = new(Ref, eorm_demo),
      Test1 = set(name, <<"John"/utf8>>, Test),
      Test2 = set(age, 18, Test1),
      Test3 = flush(Test2),
      Obj = load(Ref, eorm_demo, get(guid, Test3)),
      Test4 = delete(Test3),
      Guid = get(guid, Obj),
      Name = get(name, Obj),
      Age  = get(age, Obj),
      [
        ?_assertMatch(Guid, get(guid, Test3)),
        ?_assertMatch(Name, get(name, Test3)),
        ?_assertMatch(Age, get(age, Test3)),
        ?_assertMatch(insert, switch_action(Test2)),
        ?_assertMatch(update, switch_action(Test3)),
        ?_assertMatch(delete, switch_action(Test4))
      ]
    end
  }.

%%base_test() ->
%%  %todo Ref
%%  Test = new(test, test),
%%  Test1 = set(name, <<"John"/utf8>>, Test),
%%  Test2 = set(age, 18, Test1),
%%  ?assertMatch(<<"John"/utf8>>, get(name, Test2)),
%%  ?assertMatch(18, get(age, Test2)).
%%
%%action_test() ->

%%
%%dirty_test() ->
%%  Test = new(test, test),
%%  Test1 = set(name, <<"John"/utf8>>, Test),
%%  Test2 = set(age, 18, Test1),
%%  Test3 = flush(Test2),
%%  Test4 = set(name, <<"John2"/utf8>>, Test3),
%%  UPOp = make_update(Test4),
%%  ?assertMatch(test, UPOp#eorm_update_operation.table_name),
%%  ?assertMatch([{name, <<"John2"/utf8>>}], UPOp#eorm_update_operation.changes),
%%  Test5 = set(age, 19, Test4),
%%  UPOp2 = make_update(Test5),
%%  ?assertMatch(2, length(UPOp2#eorm_update_operation.changes)).


-endif.
