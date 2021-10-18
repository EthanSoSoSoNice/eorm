%%%-------------------------------------------------------------------
%% @doc eorm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eorm_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(Id, Type, Args), #{
  id => Id,
  start => {Id, start_link, Args},
  restart => temporary,
  shutdown => 5,
  type => Type,
  modules => []
}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      ?CHILD(eorm_env, worker, [])
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
