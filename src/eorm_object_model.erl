%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% TableInfo:
%%%   Fields :: {Name :: atom(), Type}
%%%   PrimaryKey :: FieldName
%%%
%%% @end
%%% Created : 13. 10月 2021 16:33
%%%-------------------------------------------------------------------
-module(eorm_object_model).
-author("Administrator").
-include("eorm.hrl").

%% API
-export([
  get_fields/2,
  get_primary_key/2,
  get_default_value/3
]).



-spec get_fields(Ref::atom(), Type::atom()) -> Fields :: [#eorm_field_meta{}].
get_fields(Ref, Type) ->
  Meta = eorm_env:lookup(?EORM_TABLE_META, {Ref, Type}),
  Meta#eorm_table_meta.fields.


-spec get_primary_key(Ref::atom(), Type::atom()) -> #eorm_primary_key{}.
get_primary_key(Ref, Type) ->
  Meta = eorm_env:lookup(?EORM_TABLE_META, {Ref, Type}),
  Meta#eorm_table_meta.primary_key.

-spec get_default_value(Ref::atom(), Type::atom(), Field::atom()) -> any().
get_default_value(Ref, Type, Field) ->
  Fields = get_fields(Ref, Type),
  FieldMeta = lists:keyfind(Field, #eorm_field_meta.name, Fields),
  case FieldMeta#eorm_field_meta.type of
    {_, _, Default} ->
      Default;
    {_, Default} ->
      Default
  end.
