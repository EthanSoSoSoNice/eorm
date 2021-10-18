%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 10月 2021 17:28
%%%-------------------------------------------------------------------
-module(eorm_eunit).
-author("Administrator").
-include("eorm.hrl").

%% API
-export([
  et_env/0
]).

et_env() ->
  eorm_sup:start_link(),
  emysql:start(),
  Ref = eorm_test,
  ok = eorm:run(
    Ref,
    [
      {auto_loading, false},
      {adaptor, emysql},
      {db_args, [
        {user, "root"},
        {password, "root"},
        {host, "127.0.0.1"},
        {database, "eorm_test"},
        {port, 3306},
        {encoding, utf8},
        {pool_size, 8}
      ]}
    ]
  ),
  #eorm_table_meta{} = eorm:load_table(Ref, eorm_demo),
  Ref.
