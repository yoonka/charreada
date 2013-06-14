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

-module(charreada_app).
-behaviour(application).

%% API.

-export([start/2, stop/1]).

%% API.

start(_Type, _Args) ->
    NbAcceptors = get_nb_acceptors(application:get_env(nb_acceptors)),
    Port = get_port(application:get_env(port)),
    Timeout = get_timeout(application:get_env(timeout_seconds)),
    charreada:start_http_proxy(NbAcceptors, [{port, Port}], Timeout),
    charreada_sup:start_link().

stop(_State) ->
    charreada:stop_proxy(),
    ok.

get_nb_acceptors(undefined) ->   100;
get_nb_acceptors({ok, Value}) -> Value.

get_port(undefined) ->   8080;
get_port({ok, Value}) -> Value.

get_timeout(undefined)   -> 30*1000;
get_timeout({ok, Value}) -> Value*1000.
