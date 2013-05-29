-module(charreada_config).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([add_proxy/4, remove_proxy/1,
         add_login/2, add_login/3, remove_login/1,
         allow_request/5, redirect_request/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal functions
-export([wait_for_connect/6]).

-define(SERVER, ?MODULE).
-record(config, {proxy_tid, login_tid, allow_tid}).
-opaque stringable() :: binary() | string() | atom() | integer().
-import(erlang, [phash2/1]).

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

-spec remove_login(binary()) -> ok.
remove_login(User) ->
    gen_server:call(?SERVER, {remove_login, User}).

-spec allow_request(stringable(), binary(), binary(), binary(), [] | [{user, optional}])
                   -> binary().
allow_request(Id, User, Cookie, Value, Options) ->
    gen_server:call(?SERVER, {allow, Id, User, Cookie, Value, Options}).

-spec redirect_request(
        binary(), binary(), binary(), [{binary(), binary()}],
        list({binary(), binary() | true}), function(), timeout())
                      -> ok | {error, cowboy_http:status()}.
redirect_request(Method, Host, Path, Headers, Cookies, BodyFun, Timeout) ->
    Msg = {redirect, self(), Method, Host, Path, Headers, Cookies, BodyFun, Timeout},
    gen_server:call(?SERVER, Msg).

%%% gen_server callbacks

init({TProxy, TLogin, TAllow}) ->
    {ok, #config{proxy_tid = TProxy, login_tid = TLogin, allow_tid = TAllow}}.

handle_call({add_proxy, Id, Method, Host, Port}, _From, C) ->
    NId = to_lower_binary(Id),
    true = ets:insert(C#config.proxy_tid, {phash2(NId), NId, Method, Host, Port}),
    {reply, ok, C};
handle_call({remove_proxy, Id}, _From, C) ->
    true = ets:delete(C#config.proxy_tid, phash2(to_lower_binary(Id))),
    {reply, ok, C};
handle_call({add_login, User, Password}, _From, C) ->
    Ids = ets:select(C#config.proxy_tid, [{{'$1','$2','_','_','_'}, [], [{{'$1','$2'}}]}]),
    [ insert_login(C, No, Id, phash2(User), User, Password) || {No, Id} <- Ids ],
    {reply, ok, C};
handle_call({add_login, Id, User, Password}, _From, C) ->
    NId = to_lower_binary(Id),
    insert_login(C, phash2(NId), NId, phash2(User), User, Password),
    {reply, ok, C};
handle_call({remove_login, User}, _From, C) ->
    true = ets:match_delete(C#config.login_tid, {{'_', phash2(User)}, '_'}),
    {reply, ok, C};
handle_call({allow, Id, User, Cookie, Value, Options}, _From, C) ->
    Key = {phash2(to_lower_binary(Id)), phash2(User)},
    Exists = ets:member(C#config.login_tid, Key),
    {reply, insert_allow(C, Exists, Options, Key, Cookie, Value), C};
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

insert_login(C, ProxyNo, Id, UserNo, User, Password) ->
    true = ets:insert(C#config.login_tid, {{ProxyNo, UserNo}, Id, User, Password}).

insert_allow(_C, false, [], _Key, _Cookie, _Value) ->
    {error, undef};
insert_allow(C, Exists, Options, Key, Cookie, Value) ->
    true = ets:insert(C#config.allow_tid, {Key, Cookie, Value}),
    {ok, to_subdomain(Exists, Options, Key)}.

to_subdomain(false, [{user, optional}], {ProxyNo, _UserNo}) ->
    << (omc:to_binary(ProxyNo))/binary >>;
to_subdomain(true, _Options, {ProxyNo, UserNo}) ->
    << (omc:to_binary(ProxyNo))/binary, <<"-">>/binary, (omc:to_binary(UserNo))/binary >>.

get_config(C, Host, Cookies) ->
    case parse_subdomain(binary:split(Host, <<".">>)) of
        {_ProxyNo, _UserNo} = Key -> check_redirect(C, Key, Cookies);
        undefined -> {error, 404}
    end.

parse_subdomain([Subdomain | _T]) -> get_user(binary:split(Subdomain, <<"-">>));
parse_subdomain([]) -> undefined.

get_user([Id]) -> {to_integer(Id), undefined};
get_user([Id, User]) -> {to_integer(Id), to_integer(User)};
get_user(_) -> undefined.

check_redirect(C, {_ProxyNo, undefined} = Key, _Cookies) ->
    read_config(C, Key);
check_redirect(_C, _Key, []) ->
    {error, 401};
check_redirect(C, Key, Cookies) ->
    case ets:lookup(C#config.allow_tid, Key) of
        [{Key, Cookie, Value}] ->
            case lists:keyfind(Cookie, 1, Cookies) of
                {Cookie, Value} -> read_config(C, Key);
                _ -> {error, 401}
            end;
        [] -> {error, 401}
    end.

read_config(C, {ProxyNo, _UserNo} = Key) ->
    case ets:lookup(C#config.proxy_tid, ProxyNo) of
        [Config] -> read_login(C, Key, Config);
        [] -> {error, 404}
    end.

read_login(_C, {_ProxyNo, undefined}, Config) ->
    {Config, undefined};
read_login(C, Key, Config) ->
    case ets:lookup(C#config.login_tid, Key) of
        [Login] -> {Config, Login};
        [] -> {error, 404}
    end.

wait_for_connect(Url, Headers, Method, BodyFun, Options, Timeout) ->
    ibrowse:send_req(Url, Headers, Method, BodyFun, Options, Timeout).

url({_ProxyNo, _ProxyId, Method, Host, Port}, Path) ->
    to_string(Method) ++ "://" ++ to_string(Host) ++ ":" ++ to_string(Port) ++ to_string(Path).

to_ibrowse_options(Pid, undefined) ->
    add_ibrowse_option(Pid, []);
to_ibrowse_options(Pid, {{_ProxyNo, _UserNo}, _ProxyId, User, Password}) ->
    add_ibrowse_option(Pid, [{basic_auth, {to_string(User), to_string(Password)}}]).

add_ibrowse_option(Pid, List) ->
    [{stream_to, {Pid, once}}|List].

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

to_integer(Int) when is_binary(Int) ->
    list_to_integer(binary_to_list(Int)).
