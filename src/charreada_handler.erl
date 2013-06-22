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

-module(charreada_handler).

%% Cowboy handler

-export([onrequest/2]).
-export([init/3]).
-export([info/3]).
-export([terminate/3]).

onrequest(Req, Timeout) ->
    proxy_host(cowboy_req:header(<<"host">>, Req), Timeout).

proxy_host({undefined, Req}, _Timeout) ->
    {ok, ReplyReq} = cowboy_req:reply(404, Req),
    ReplyReq;
proxy_host({Host, Req}, Timeout) ->
    {Method, Req} = cowboy_req:method(Req),
    {HostUrl, Req} = cowboy_req:host_url(Req),
    {Url, Req} = cowboy_req:url(Req),
    UrlLen = byte_size(Url),
    PathUrl = binary:part(Url, {UrlLen, -(UrlLen - byte_size(HostUrl))}),
    {OrgHeaders, Req} = cowboy_req:headers(Req),
    NoHostHeaders = lists:keydelete(<<"host">>, 1, OrgHeaders),
    NoConnHeaders = lists:keydelete(<<"connection">>, 1, NoHostHeaders),
    {Cookies, CookiesReq} = cowboy_req:cookies(Req),
    BodyFun = {fun post_body/1, self()},
    Resp = charreada_config:redirect_request(
             Method, Host, PathUrl, NoConnHeaders, Cookies, BodyFun, Timeout),
    handle_redirect(Resp, CookiesReq, Timeout).

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
    Fun =
        fun(Socket, Transport) ->
                stream_reply({RequestId, Transport, Socket, Length}, Length)
        end,
    {ok, ReplyReq} = cowboy_req:reply(list_to_integer(Code), Headers, {Length, Fun}, Req),
    {loop, ReplyReq, {Timeout, Length}};
info({ibrowse_async_response, _RequestId, []}, Req, State) ->
    {loop, Req, State};
info({ibrowse_async_response, _RequestId, {error, _}}, Req, State) ->
    {ok, Req, State};
info({ibrowse_async_response_end, _RequestId}, Req, State) ->
    {ok, Req, State};
info({cowboy_req, resp_sent}, Req, State) ->
    {loop, Req, State};
info({error, _Reason}, Req, State) ->
    reply(502, Req, State).

terminate(_Reason, _Req, _State) ->
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
