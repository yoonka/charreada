-module(charreada_config).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([add_proxy/1, remove_proxy/1, redirect_req/6]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal functions
-export([wait_for_connect/6]).

-define(SERVER, ?MODULE).

%%% API
start_link(Tid) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Tid, []).

-spec add_proxy ({atom(), term()}) -> ok; ({binary(), term()}) -> ok.
add_proxy(Proxy) ->
    gen_server:cast(?SERVER, {add, Proxy}).

-spec remove_proxy (atom()) -> ok; (binary()) -> ok.
remove_proxy(Name) ->
    gen_server:cast(?SERVER, {remove, Name}).

-spec redirect_req(
        binary(), binary(), binary(), [{binary(), binary()}], function(), timeout()) ->
                          ok | {error, cowboy_http:status()}.
redirect_req(Method, Host, Path, Headers, BodyFun, Timeout) ->
    Msg = {redirect, self(), Method, Host, Path, Headers, BodyFun, Timeout},
    gen_server:call(?SERVER, Msg).

%%% gen_server callbacks

init(Tid) ->
    {ok, Tid}.

handle_call({redirect, Pid, Method, Host, Path, Headers, BodyFun, Timeout}, _From, Tid) ->
    Proxy = match_host(Tid, binary:split(Host, <<".">>)),
    Reply = start_redirect(Proxy, Pid, Method, Path, Headers, BodyFun, Timeout),
    {reply, Reply, Tid}.

handle_cast({add, Proxy}, Tid) ->
    ets:insert(Tid, normalize(Proxy)),
    {noreply, Tid};
handle_cast({remove, Name}, Tid) ->
    ets:delete(Tid, normalize_key(Name)),
    {noreply, Tid}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal methods

normalize({Name, Transport, Host, Port, User, Password}) ->
    {to_lower_binary(Name), Transport, Host, Port, User, Password}.

normalize_key(Name) ->
    to_lower_binary(Name).

match_host(Tid, [Subdomain | _T]) ->
    ets:lookup(Tid, to_lower_binary(Subdomain));
match_host(_Tid, []) ->
    [].

start_redirect([], _Pid, _Method, _Path, _Headers, _BodyFun, _Timeout) ->
    {error, 404};
start_redirect([{_, Transport, Host, Port, User, Password}],
               Pid, OrgMethod, Path, OrgHeaders, BodyFun, Timeout) ->
    Url = url(Transport, Host, Port, Path),
    Headers = to_strings(OrgHeaders, []),
    Method = to_atom(OrgMethod),
    Options = to_ibrowse_options(Pid, User, Password),
    Params = [Url, Headers, Method, BodyFun, Options, Timeout],
    proc_lib:spawn(?MODULE, wait_for_connect, Params),
    ok.

wait_for_connect(Url, Headers, Method, BodyFun, Options, Timeout) ->
    ibrowse:send_req(Url, Headers, Method, BodyFun, Options, Timeout).

url(Transport, Host, Port, Path) ->
    to_string(Transport) ++ "://" ++ to_string(Host) ++ ":" ++ to_string(Port) ++ to_string(Path).

to_lower_binary(Value) when is_binary(Value) ->
    list_to_binary(string:to_lower(binary_to_list(Value)));
to_lower_binary(Value) when is_atom(Value) ->
    list_to_binary(string:to_lower(atom_to_list(Value)));
to_lower_binary(Value) when is_list(Value) ->
    list_to_binary(string:to_lower(Value)).

to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_string({IP1, IP2, IP3, IP4}) ->
    to_string(IP1) ++ "." ++ to_string(IP2) ++ "." ++ to_string(IP3) ++ "." ++ to_string(IP4).

to_atom(<<"GET">>) -> get;
to_atom(<<"POST">>) -> post;
to_atom(<<"HEAD">>) -> head;
to_atom(<<"OPTIONS">>) -> options;
to_atom(<<"PUT">>) -> put;
to_atom(<<"DELETE">>) -> delete;
to_atom(<<"TRACE">>) -> trace;
to_atom(<<"MKCOL">>) -> mkcol;
to_atom(<<"PROPFIND">>) -> propfind;
to_atom(<<"PROPPATCH">>) -> proppatch;
to_atom(<<"LOCK">>) -> lock;
to_atom(<<"UNLOCK">>) -> unlock;
to_atom(<<"MOVE">>) -> move;
to_atom(<<"COPY">>) -> copy.

to_strings([{Key, Value}|T], Acc) ->
    to_strings(T, [{to_string(Key), to_string(Value)}|Acc]);
to_strings([], Acc) ->
    lists:reverse(Acc).

to_ibrowse_options(Pid, undefined, _Password) ->
    to_ibrowse_options([], Pid);
to_ibrowse_options(Pid, _User, undefined) ->
    to_ibrowse_options([], Pid);
to_ibrowse_options(Pid, User, Password) ->
    to_ibrowse_options([{basic_auth, {to_string(User), to_string(Password)}}], Pid).

to_ibrowse_options(Acc, Pid) ->
    [{stream_to, {Pid, once}}|Acc].
