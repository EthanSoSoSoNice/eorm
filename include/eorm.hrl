-ifndef(EORM).
-define(EORM, true).

-record(eorm_update_operation, {
  table_name = undefined :: atom(),
  key_name = undefined :: atom(),
  key_value = undefined :: any(),
  changes = [] :: []
}).

-record(eorm_insert_operation, {
  table_name = undefined :: atom(),
  fields_name = [] :: [],
  records = [] :: []
}).

-record(eorm_delete_operation, {
  table_name = undefined :: atom(),
  key_name = undefined :: atom(),
  key_value = undefined :: any()
}).

-record(eorm_select_operation, {
  table_name = undefined :: atom(),
  key_name = undefined :: atom(),
  key_value = undefined :: any()
}).

-record(eorm_ok_packet, {
  affected_rows = 0 :: integer(),       %% affect rows
  last_insert_id = 0 :: integer(),      %% last insert-id
  warnings = 0 :: integer(),            %% number of warnings
  info = <<>> :: binary()               %% human readable status information
}).

-record(eorm_result_packet, {
  field_list = [],
  rows = [],
  extra
}).

-record(eorm_err_packet, {
  error_code = 0 :: integer(),
  error_message = <<>> :: binary()      %% human readable error message
}).


-type eorm_field_type() ::
  {char, Len::integer(), Default::binary()} | %% char(Len)
  {varchar, Max::integer(), Default::binary()} | %% varchar(Max)
  {int32, Default::integer()} |
  {uint32, Default::non_neg_integer()} |
  {int16, Default::integer()} |
  {uint16, Default::non_neg_integer()} |
  {int8, Default::integer()} |
  {uint8, Default::non_neg_integer()} |
  {blob, Default::binary()} |
  {text, Default::binary()}.

-define(EORM_ENV_TAB, eorm_env).
-define(EORM_TABLE_META, eorm_table_meta).
-define(EORM_TABLE_FIELDS, eorm_table_fields). %%

-record(eorm_field_meta, {
  name :: atom(),
  type :: undefined | eorm_field_type()
}).

-record(eorm_primary_key, {
  field = none :: atom(),
  auto_increment = false :: boolean()       %% auto increment key
}).

-record(eorm_table_meta, {
  table_name  :: atom(),
  fields = [] :: [#eorm_field_meta{}],
  primary_key = undefined :: #eorm_primary_key{} | undefined
}).

-endif.