-module(charreada_sup).
-behaviour(supervisor).

%% API.

-export([start_link/0]).

%% Supervisor callbacks

-export([init/1]).

%% Helper macro for declaring supervisor worker
-define(WORKER(I, P), {I, {I, start_link, [P]}, permanent, 5000, worker, [I]}).

%% API

start_link() ->
	supervisor:start_link(?MODULE, []).

%% Supervisor callbacks

init([]) ->
    %% The supervisor owns the tables
    TProxy = ets:new(charreada_proxy, [public, {read_concurrency, true}]),
    TLogin = ets:new(charreada_login, [public, {read_concurrency, true}]),
    Procs = [?WORKER(charreada_config, {TProxy, TLogin})],
    {ok, {{one_for_one, 5, 15}, Procs}}.
