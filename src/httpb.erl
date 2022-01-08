%%%
%%% HTTP Basic Client
%%%
%%% Need something WebSocket like where we have control over the socket
%%% and can continue to do bi-directional communication if necessary.
%%% inets:httpc only allows one-way traffic and other clients do not
%%% appear to provide the low level control required.
%%%
-module(httpb).
-export([
    request/4, request/5, response/1, response/2,
    is_chunked/1, is_keep_alive/1, send_chunk/2, recv_chunk/1, recv_chunk/2,
    close/1, recv/2, recv/3, send/2, getopts/2, setopts/2, controlling_process/2
]).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(DEFAULT_TIMEOUT, 30000).

-type url()             :: string() | binary().
-type body()            :: binary().
-type headers()         :: map().
-type reason()          :: term().
-type error()           :: {error, reason()}.
-type scheme()          :: http | https.
-type method()          :: get | head | options | post | put | delete.
-type result()          :: #{status => integer(), headers => headers(), body => body()}.
-type socket()          :: gen_tcp:socket() | ssl:sslsocket().
-type connection()      :: #{scheme => scheme(), host => string() | binary(), port => non_neg_integer(), socket => socket()}.

-type ret_ok()          :: ok | error().
-type ret_data()        :: {ok, binary()} | error().
-type ret_conn()        :: {ok, connection()} | error().
-type ret_result()      :: {ok, result()} | error().

-spec request(Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body()) -> ret_conn().
request(Method, Url, Hdrs, Body) when is_list(Url) ->
    request(Method, list_to_binary(Url), Hdrs, Body);
request(Method, Url, Hdrs, Body) ->
    {ok, {Scheme, _UserInfo, Host, Port, _Path, _Query}} = http_uri:parse(Url),
    case connect(Scheme, Host, Port) of
    {ok, Socket} ->
        Conn = #{scheme => Scheme, host => Host, port => Port, socket => Socket},
        request(Conn, Method, Url, Hdrs, Body);
    Other ->
        Other
    end.

-spec request(Conn :: connection(), Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body()) -> ret_conn().
request(Conn, Method, Url, Hdrs, Body) when is_list(Url) ->
    request(Conn, Method, list_to_binary(Url), Hdrs, Body);
request(#{host := Host, port := Port} = Conn, Method, Url, Hdrs, Body) ->
    {ok, {_Scheme, _UserInfo, _Host, _Port, Path, Query}} = http_uri:parse(Url),
    Req0 = <<(method(Method))/binary, " ", (path(Path, Query))/binary, " HTTP/1.1\r\n">>,
    Req1 = headers(Req0, Hdrs#{host => <<Host/binary, $:, (integer_to_binary(Port))/binary>>}),
    case send(Conn, Req1) of
    ok ->
        case send(Conn, Body) of
        ok ->
            ok = setopts(Conn, [{active, once}, {packet, http_bin}]),
            {ok, Conn};
        Other ->
            Other
        end;
    Other ->
        Other
    end.

-spec method(Method :: method()) -> binary().
method(get) ->
    <<"GET">>;
method(head) ->
    <<"HEAD">>;
method(options) ->
    <<"OPTIONS">>;
method(post) ->
    <<"POST">>;
method(put) ->
    <<"PUT">>;
method(delete) ->
    <<"DELETE">>.

-spec path(Path :: binary(), Query :: binary() | undefined) -> binary().
path(Path, undefined) ->
    Path;
path(Path, <<>>) ->
    Path;
path(Path, Query) ->
    <<Path/binary, $?, Query/binary>>.

-spec connect(Scheme :: scheme(), Host :: binary(), Port :: non_neg_integer()) -> {ok, socket()} | error().
connect(Scheme, Host, Port) ->
    connect(Scheme, Host, Port, [binary], ?DEFAULT_TIMEOUT).

-spec connect(Scheme :: scheme(), Host :: string() | binary(), Port :: non_neg_integer(), Options :: list(), Timeout :: timeout()) -> {ok, socket()} | error().
connect(Scheme, Host, Port, Options, Timeout) when is_binary(Host) ->
    connect(Scheme, binary_to_list(Host), Port, Options, Timeout);
connect(http, Host, Port, Options, Timeout) ->
    gen_tcp:connect(Host, Port, Options, Timeout);
connect(https, Host, Port, Options, Timeout) ->
    ssl:start(),
    ssl:connect(Host, Port, Options, Timeout).

-spec close(Conn :: connection()) -> ret_ok().
close(#{scheme := http, socket := Socket}) ->
    gen_tcp:close(Socket);
close(#{scheme := https, socket := Socket}) ->
    ssl:close(Socket).

-spec getopts(Conn :: connection(), Options :: list()) -> ret_ok().
getopts(#{scheme := http, socket := Socket}, Options) ->
    inet:getopts(Socket, Options);
getopts(#{scheme := https, socket := Socket}, Options) ->
    ssl:getopts(Socket, Options).

-spec setopts(Conn :: connection(), Options :: list()) -> ret_ok().
setopts(#{scheme := http, socket := Socket}, Options) ->
    inet:setopts(Socket, Options);
setopts(#{scheme := https, socket := Socket}, Options) ->
    ssl:setopts(Socket, Options).

-spec send(Conn :: connection(), Data :: binary()) -> ret_ok().
send(_Conn, <<>>) ->
    ok;
send(#{scheme := http, socket := Socket}, Data) ->
    gen_tcp:send(Socket, Data);
send(#{scheme := https, socket := Socket}, Data) ->
    ssl:send(Socket, Data).

-spec recv(Conn :: connection(), Length :: non_neg_integer()) -> ret_data().
recv(Conn, Length) ->
    recv(Conn, Length, infinity).

-spec recv(Conn :: connection(), Length :: non_neg_integer(), Timeout :: timeout()) -> ret_data().
recv(#{scheme := http, socket := Socket}, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout);
recv(#{scheme := https, socket := Socket}, Length, Timeout) ->
    ssl:recv(Socket, Length, Timeout).

-spec controlling_process(Conn :: connection(), Pid :: pid()) -> ret_ok().
controlling_process(#{scheme := http, socket := Socket}, Pid) ->
    gen_tcp:controlling_process(Socket, Pid);
controlling_process(#{scheme := https, socket := Socket}, Pid) ->
    ssl:controlling_process(Socket, Pid).

-spec field_to_atom(Field :: atom() | string()) -> atom().
field_to_atom(Field) when is_atom(Field) ->
    field_to_atom(atom_to_list(Field));
field_to_atom(Field) when is_binary(Field) ->
    field_to_atom(binary_to_list(Field));
field_to_atom(Field) ->
    Field1 = string:to_lower(Field),
    Field2 = lists:flatten(string:replace(Field1, "-", "_", all)),
    list_to_atom(Field2).

-spec atom_to_field(Field :: atom()) -> binary().
atom_to_field(Field) ->
    Field1 = atom_to_list(Field),
    Field2 = lists:flatten(string:replace(Field1, "_", "-", all)),
    list_to_binary(Field2).

-spec headers(Req :: binary(), Hdrs :: headers()) -> binary().
headers(Req, Hdrs) ->
    Bs = maps:fold(fun
        (Key, Value, Acc) ->
            Field = atom_to_field(Key),
            <<Acc/binary, Field/binary, ": ", Value/binary, "\r\n">>
    end, Req, Hdrs),
    <<Bs/binary, "\r\n">>.

-spec is_keep_alive(Result_Or_Headers :: result() | headers()) -> boolean().
is_keep_alive(#{headers := Headers}) ->
    is_keep_alive(Headers);
is_keep_alive(#{connection := <<"close">>}) ->
    false;
is_keep_alive(_Hdrs) ->
    true.

-spec is_chunked(Result_Or_Headers :: result() | headers()) -> boolean().
is_chunked(#{headers := Headers}) ->
    is_chunked(Headers);
is_chunked(#{transfer_encoding := <<"chunked">>}) ->
    true;
is_chunked(_Hdrs) ->
    false.

-spec response(Conn :: connection()) -> ret_result().
response(Conn) ->
    response(Conn, ?DEFAULT_TIMEOUT).

-spec response(Conn :: connection(), Timeout :: timeout()) -> ret_result().
response(Conn, Timeout) ->
    response(Conn, Timeout, #{status => 0, headers => #{}}).

-spec response(Conn :: connection(), Timeout :: timeout(), Res :: result()) -> ret_result().
response(Conn, Timeout, Res) ->
    receive
    {_Type, _Socket, {http_response, _, Status, _}} ->
        % _Type :: http | ssl
        ok = setopts(Conn, [{active, once}]),
        response(Conn, Timeout, Res#{status => Status});
    {_Type, _Socket, {http_header, _, Key, _, Value}} ->
        Key1 = field_to_atom(Key),
        Hdrs = maps:get(headers, Res),
        ok = setopts(Conn, [{active, once}]),
        response(Conn, Timeout, Res#{headers => Hdrs#{Key1 => Value}});
    {_Type, _Socket, {http_error, Data}}  ->
        % Switching to raw packets can still result in some data.
        Body = maps:get(body, Res, <<>>),
        ok = setopts(Conn, [{active, once}]),
        response(Conn, Timeout, Res#{body => <<Body/binary, Data/binary>>});
    {_Type, _Socket, http_eoh} ->
        case is_chunked(Res) of
        false ->
            % Read as much of the body as possible.
            ok = setopts(Conn, [{active, once}, {packet, raw}]),
            response(Conn, Timeout, Res);
        true ->
            % Stop reading before first chunk.
            {ok, Res}
        end;
    {ssl, _Socket, Data} ->
        Body = maps:get(body, Res, <<>>),
        {ok, Res#{body => <<Body/binary, Data/binary>>}};
    {ssl_error, _Socket, Reason} ->
        close(Conn),
        {error, Reason};
    {ssl_closed, _Socket} ->
        % The server write end can close before we've read all the data.
        response(Conn, Timeout, Res);
    {tcp, _Socket, Data} ->
        Body = maps:get(body, Res, <<>>),
        {ok, Res#{body => <<Body/binary, Data/binary>>}};
    {tcp_error, _Socket, Reason} ->
        close(Conn),
        {error, Reason};
    {tcp_closed, _Socket} ->
        % The server write end can close before we've read all the data.
        response(Conn, Timeout, Res);
    Other ->
        {error, Other}
    after Timeout ->
        {error, {timeout, Timeout}}
    end.

-spec recv_chunk(Conn :: connection()) -> ret_data().
recv_chunk(Conn) ->
    recv_chunk(Conn, ?DEFAULT_TIMEOUT).

-spec recv_chunk(Conn :: connection(), Timeout :: timeout()) -> ret_data().
recv_chunk(Conn, Timeout) ->
    ok = setopts(Conn, [{active, once}, {packet, line}]),
    receive
    {tcp_error, _Socket, Reason} ->
        close(Conn),
        {error, Reason};
    {tcp_closed, _Socket} ->
        close(Conn),
        {ok, <<>>};
    {tcp, _Socket, Line} ->
        Hex = string:trim(Line),
        Len = binary_to_integer(Hex, 16),
        ok = setopts(Conn, [{packet, raw}]),
        recv(Conn, Len + 2, Timeout);
    Other ->
        {error, Other}
    after Timeout ->
        {error, {timeout, Timeout}}
    end.

-spec send_chunk(Conn :: connection(), Data :: binary()) -> ret_ok().
send_chunk(Conn, Data) ->
    Hex = integer_to_binary(byte_size(Data), 16),
    case send(Conn, <<Hex/binary, "\r\n">>) of
    ok -> send(Conn, <<Data/binary, "\r\n">>);
    Other -> Other
    end.
