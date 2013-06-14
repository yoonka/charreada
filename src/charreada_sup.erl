%% Copyright (c) 2013, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% Redistributions of source code must retain the above copyright notice,
%% this list of conditions and the following disclaimer.
%% Redistributions in binary form must reproduce the above copyright notice,
%% this list of conditions and the following disclaimer in the documentation
%% and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
    TAllow = ets:new(charreada_allow, [public, {read_concurrency, true}]),
    Procs = [?WORKER(charreada_config, {TProxy, TLogin, TAllow})],
    {ok, {{one_for_one, 5, 15}, Procs}}.
