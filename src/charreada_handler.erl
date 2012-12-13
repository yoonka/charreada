-module(charreada_handler).

%% Cowboy handler

-export([onrequest/2]).
-export([init/3]).
-export([info/3]).
-export([terminate/2]).

onrequest(Req, Timeout) ->
    proxy_host(cowboy_req:header(<<"host">>, Req), Timeout).

proxy_host({undefined, Req}, _Timeout) ->
    {ok, ReplyReq} = cowboy_req:reply(404, Req),
    ReplyReq;
proxy_host({Host, Req}, Timeout) ->
    {Method, Req} = cowboy_req:method(Req),
    {Path, Req} = cowboy_req:path(Req),
    {OrgHeaders, Req} = cowboy_req:headers(Req),
    Headers = lists:keydelete(<<"host">>, 1, OrgHeaders),
    BodyFun = {fun post_body/1, self()},
    Resp = charreada_config:redirect_req(Method, Host, Path, Headers, BodyFun, Timeout),
    handle_redirect(Resp, Req, Timeout).

handle_redirect(ok, Req, Timeout) ->
    cowboy_req:set_meta(timeout, Timeout, Req);
handle_redirect({error, Status}, Req, _Timeout) ->
    {ok, ReplyReq} = cowboy_req:reply(Status, Req),
    ReplyReq.

init(_Transport, OrgReq, []) ->
    {Timeout, Req} = cowboy_req:meta(timeout, OrgReq),
    {loop, Req, {Timeout, undefined}, Timeout}.

info({ibrowse_get_body, Pid}, Req, State) ->
    stream(cowboy_req:stream_body(Req), Req, Pid, State);
info({ibrowse_async_headers, RequestId, Code, HeadersOrg}, Req, {Timeout, undefined}) ->
    {Headers, Length} = process_headers(HeadersOrg, [], 0),
    {ok, Transport, Socket} = cowboy_req:transport(Req),
    Fun = fun() -> stream_reply({RequestId, Transport, Socket, Length}, Length) end,
    {ok, ReplyReq} = cowboy_req:reply(list_to_integer(Code), Headers, {Length, Fun}, Req),
    {loop, ReplyReq, {Timeout, Length}};
info({ibrowse_async_response, _RequestId, []}, Req, State) ->
    {loop, Req, State};
info({ibrowse_async_response_end, _RequestId}, Req, State) ->
    {ok, Req, State};
info({cowboy_req, resp_sent}, Req, State) ->
    {loop, Req, State};
info({error, _Reason}, Req, State) ->
    reply(502, Req, State).

terminate(_Req, _State) ->
    ok.

post_body(Pid) ->
    Pid ! {ibrowse_get_body, self()},
    receive
        {ok, Data} ->
            {ok, Data, Pid};
        eof ->
            eof
    end.

stream({done, Req}, _Req, Pid, State) ->
    Pid ! eof,
    {loop, Req, State};
stream({ok, Data, Req}, _Req, Pid, State) ->
    Pid ! {ok, Data},
    {loop, Req, State};
stream({error, _Reason}, Req, Pid, State) ->
    Pid ! eof,
    reply(502, Req, State).

process_headers([{Key, Value}|T], Acc, Length) ->
    BKey = list_to_binary(string:to_lower(Key)),
    process_headers(BKey, Value, T, Acc, Length);
process_headers([], Acc, Length) ->
    {lists:reverse(Acc), Length}.

process_headers(<<"content-length">>, Value, Headers, Acc, _Length) ->
    process_headers(Headers, Acc, list_to_integer(Value));
process_headers(Key, Value, Headers, Acc, Length) ->
    process_headers(Headers, [{Key, Value}|Acc], Length).

stream_reply({RequestId, Transport, Socket, _Length} = ReqInfo, ToSend) ->
    ibrowse:stream_next(RequestId),
    receive
        {ibrowse_async_response, RequestId, Data} ->
            ChunkLength = length(Data),
            Result = Transport:send(Socket, Data),
            check_send(Result, ReqInfo, ToSend, ChunkLength)
    end.

check_send(ok, ReqInfo, ToSend, ChunkLength) ->
    check_send(ReqInfo, ToSend - ChunkLength);
check_send({error, _}, {RequestId, Transport, Socket, Length}, ToSend, _ChunkLength) ->
    ibrowse:stream_close(RequestId),
    Transport:close(Socket),
    {sent, Length - ToSend}.

check_send({_RequestId, _Transport, _Socket, Length}, ToSend) when ToSend =< 0 ->
    {sent, Length};    
check_send(ReqInfo, ToSend) ->
    stream_reply(ReqInfo, ToSend).

reply({ok, Req}, State) ->
    {ok, Req, State}.

reply(Code, Req, State) ->
    reply(cowboy_req:reply(Code, Req), State).
