-module(charreada).

%% API

-export([start_http_proxy/3, stop_proxy/0]).

start_http_proxy(NbAcceptors, TransOpts, Timeout) ->
    Dispatch = [{'_', [ {'_', charreada_handler, []} ]} ],
    Fun = fun(Req) -> charreada_handler:onrequest(Req, Timeout) end,
    ProtoOpts = [ {env, [{dispatch, Dispatch}]}, {onrequest, Fun} ],
    {ok, _} = cowboy:start_http(http, NbAcceptors, TransOpts, ProtoOpts),
    ok.

stop_proxy() ->
    ok = cowboy:stop_listener(http),
    ok.
