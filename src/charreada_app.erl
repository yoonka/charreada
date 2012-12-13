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
