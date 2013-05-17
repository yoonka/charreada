-module(charreada_config).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([add_proxy/4, remove_proxy/1,
         add_login/2, add_login/3, is_login/2, remove_login/1,
         allow_req/4, redirect_req/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal functions
-export([wait_for_connect/6]).

-define(SERVER, ?MODULE).

-record(config, {proxy_tid, login_tid, allow_tid}).

-opaque stringable() :: binary() | string() | atom() | integer().

%%% API
start_link(Tables) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Tables, []).

-spec add_proxy(stringable(), atom(), inet:ip_address(), inet:port_number()) -> ok.
add_proxy(Id, Method, Host, Port) ->
    gen_server:call(?SERVER, {add_proxy, Id, Method, Host, Port}).

-spec remove_proxy(stringable()) -> ok.
remove_proxy(Id) ->
    gen_server:call(?SERVER, {remove_proxy, Id}).

-spec add_login(binary(), binary()) -> ok.
add_login(User, Password) ->
    gen_server:call(?SERVER, {add_login, User, Password}).

-spec add_login(stringable(), binary(), binary()) -> ok.
add_login(Id, User, Password) ->
    gen_server:call(?SERVER, {add_login, Id, User, Password}).

-spec is_login(stringable(), binary()) -> boolean().
is_login(Id, User) ->
    gen_server:call(?SERVER, {is_login, Id, User}).

-spec remove_login(binary()) -> ok.
remove_login(User) ->
    gen_server:call(?SERVER, {remove_login, User}).

-spec allow_req(stringable(), binary(), binary(), binary()) -> non_neg_integer().
allow_req(Id, User, Cookie, Value) ->
    gen_server:call(?SERVER, {allow, Id, User, Cookie, Value}).

-spec redirect_req(
        binary(), binary(), binary(),
        [{binary(), binary()}],
        list({binary(), binary() | true}), function(), timeout()) ->
                          ok | {error, cowboy_http:status()}.
redirect_req(Method, Host, Path, Headers, Cookies, BodyFun, Timeout) ->
    Msg = {redirect, self(), Method, Host, Path, Headers, Cookies, BodyFun, Timeout},
    gen_server:call(?SERVER, Msg).

%%% gen_server callbacks

init({TProxy, TLogin, TAllow}) ->
    {ok, #config{proxy_tid = TProxy, login_tid = TLogin, allow_tid = TAllow}}.

handle_call({add_proxy, Id, Method, Host, Port}, _From, C) ->
    true = ets:insert(C#config.proxy_tid, {to_lower_binary(Id), Method, Host, Port}),
    {reply, ok, C};
handle_call({remove_proxy, Id}, _From, C) ->
    true = ets:delete(C#config.proxy_tid, to_lower_binary(Id)),
    {reply, ok, C};
handle_call({add_login, User, Password}, _From, C) ->
    Ids = ets:select(C#config.proxy_tid, [{{'$1', '_', '_', '_'}, [], ['$1']}]),
    [ insert_login(C, Id, User, Password) || Id <- Ids ],
    {reply, ok, C};
handle_call({add_login, Id, User, Password}, _From, C) ->
    insert_login(C, Id, User, Password),
    {reply, ok, C};
handle_call({is_login, Id, User}, _From, C) ->
    {reply, ets:member(C#config.login_tid, {to_lower_binary(Id), User}), C};
handle_call({remove_login, User}, _From, C) ->
    true = ets:match_delete(C#config.login_tid, {{'_', User}, '_'}),
    {reply, ok, C};
handle_call({allow, Id, User, Cookie, Value}, _From, C) ->
    true = ets:insert(C#config.allow_tid, {{to_lower_binary(Id), User}, Cookie, Value}),
    {reply, ok, C};
handle_call({redirect, Pid, Method, Host, Path, H, Cookies, BodyFun, Timeout}, _From, C) ->
    case get_config(C, Host, Cookies) of
        {error, _} = Error ->
            {reply, Error, C};
        {Config, Login} ->
            Url = url(Config, Path),
            Headers = [ {to_string(Key), to_string(Value)} || {Key, Value} <- H ],
            Options = to_ibrowse_options(Pid, Login),
            Params = [Url, Headers, to_atom(Method), BodyFun, Options, Timeout],
            proc_lib:spawn(?MODULE, wait_for_connect, Params),
            {reply, ok, C}
    end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal methods

insert_login(C, Id, User, Password) ->
    true = ets:insert(C#config.login_tid, {{to_lower_binary(Id), User}, Password}).

get_config(C, Host, Cookies) ->
    case parse_subdomain(binary:split(Host, <<".">>)) of
        {_Id, _User} = Redirect -> check_redirect(C, Redirect, Cookies);
        undefined -> {error, 404}
    end.

parse_subdomain([Subdomain | _T]) -> get_user(binary:split(Subdomain, <<"-">>));
parse_subdomain([]) -> undefined.

get_user([Id]) -> {to_lower_binary(Id), undefined};
get_user([Id, User]) -> {to_lower_binary(Id), User};
get_user(_) -> undefined.

check_redirect(C, {_Id, undefined} = Redirect, _Cookies) ->
    read_config(C, Redirect);
check_redirect(_C, _Redirect, []) ->
    {error, 401};
check_redirect(C, Redirect, Cookies) ->
    case ets:lookup(C#config.allow_tid, Redirect) of
        [{Redirect, Cookie, Value}] ->
            case lists:keyfind(Cookie, 1, Cookies) of
                {Cookie, Value} -> read_config(C, Redirect);
                _ -> {error, 401}
            end;
        [] -> {error, 401}
    end.

read_config(C, {Id, _User} = Redirect) ->
    case ets:lookup(C#config.proxy_tid, Id) of
        [Config] -> read_login(C, Redirect, Config);
        [] -> {error, 404}
    end.

read_login(_C, {_Id, undefined}, Config) ->
    {Config, undefined};
read_login(C, Redirect, Config) ->
    case ets:lookup(C#config.login_tid, Redirect) of
        [Login] -> {Config, Login};
        [] -> {error, 404}
    end.

wait_for_connect(Url, Headers, Method, BodyFun, Options, Timeout) ->
    ibrowse:send_req(Url, Headers, Method, BodyFun, Options, Timeout).

url({_, Method, Host, Port}, Path) ->
    to_string(Method) ++ "://" ++ to_string(Host) ++ ":" ++ to_string(Port) ++ to_string(Path).

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

to_ibrowse_options(Pid, undefined) ->
    add_ibrowse_option(Pid, []);
to_ibrowse_options(Pid, {{_, User}, Password}) ->
    add_ibrowse_option(Pid, [{basic_auth, {to_string(User), to_string(Password)}}]).

add_ibrowse_option(Pid, List) ->
    [{stream_to, {Pid, once}}|List].
