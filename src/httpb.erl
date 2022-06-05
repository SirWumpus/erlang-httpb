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
    is_chunked/1, is_keep_alive/1, content_length/1, send_chunk/2, recv_chunk/1, recv_chunk/2,
    close/1, recv/2, recv/3, send/2, getopts/2, setopts/2, controlling_process/2,
    one_request/4, one_request/5, body_length/1
]).

-ifdef(TEST).
-export([has_body/2]).
-endif.

-define(DEFAULT_TIMEOUT, 30000).

-type url()             :: string() | binary().
-type body()            :: binary().
-type headers()         :: #{atom() => binary()}.
-type reason()          :: term().
-type error()           :: {error, reason()}.
-type scheme()          :: http | https.
-type method()          :: delete | get | head | options | post | put | trace.
-type result()          :: #{status => integer(), headers => headers(), body => body()}.
-type socket()          :: gen_tcp:socket() | ssl:sslsocket().
-type options()         :: #{socket_opts => proplists:proplist(), timeout => timeout()}.
-type connection()      :: #{scheme => scheme(), host => string() | binary(), port => non_neg_integer(), socket => socket(), method => method()}.

-type ret_ok()          :: ok | error().
-type ret_data()        :: {ok, binary()} | error().
-type ret_conn()        :: {ok, connection()} | error().
-type ret_result()      :: {ok, result()} | error().

-spec one_request(Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body()) -> ret_result().
one_request(Method, Url, Hdrs, Body) ->
    one_request(Method, Url, Hdrs, Body, #{}).

-spec one_request(Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body(), Options :: options()) -> ret_result().
one_request(Method, Url, Hdrs, Body, Options) ->
    case request(Method, Url, Hdrs#{connection => <<"close">>}, Body, Options) of
    {ok, Conn} ->
        Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
        Result = response(Conn, Timeout),
        close(Conn),
        Result;
    Other ->
        Other
    end.

-spec request(Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body()) -> ret_conn().
request(Method, Url, Hdrs, Body) ->
    request(Method, Url, Hdrs, Body, #{socket_opts => []}).

-spec request
    (Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body(), Options :: options()) -> ret_conn() ;
    (Conn :: connection(), Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body()) -> ret_conn() .

request(Method, Url, Hdrs, Body, Options) when is_atom(Method) andalso is_list(Url) ->
    request(Method, list_to_binary(Url), Hdrs, Body, Options);
request(Method, Url, Hdrs, Body, Options) when is_atom(Method) ->
    SocketOpts = maps:get(socket_opts, Options, []),
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    UriMap = uri_string:parse(Url),
    Scheme = binary_to_atom(maps:get(scheme, UriMap)),
    Host = maps:get(host, UriMap, <<"localhost">>),
    Port = maps:get(port, UriMap, scheme_to_port(Scheme)),
    case connect(Scheme, Host, Port, SocketOpts, Timeout) of
    {ok, Socket} ->
        Conn = #{scheme => Scheme, host => Host, port => Port, socket => Socket},
        case request(Conn, Method, Url, Hdrs, Body) of
        {error, Reason} ->
            % Initial request failed, cleanup connection.
            close(Conn),
            {error, Reason};
        Result ->
            Result
        end;
    Other ->
        Other
    end;

request(Conn, Method, Url, Hdrs, Body) when is_map(Conn) andalso is_list(Url) ->
    request(Conn, Method, list_to_binary(Url), Hdrs, Body);
request(#{host := Host, port := Port} = Conn, Method, Url, Hdrs, Body) when is_map(Conn) ->
    UrlMap = uri_string:parse(Url),
    Path1 = case {Method, maps:get(path, UrlMap, <<"/">>)} of
    {options, <<"/*">>} ->
        <<"*">>;
    {_, <<>>} ->
        <<"/">>;
    {_, Path0} ->
        Path0
    end,
    Query = maps:get(query, UrlMap, <<>>),
    Req0 = <<(method(Method))/binary, " ", (path(Path1, Query))/binary, " HTTP/1.1\r\n">>,
    Req1 = headers(Req0, Hdrs#{host => <<Host/binary, $:, (integer_to_binary(Port))/binary>>}),
    case send(Conn, Req1) of
    ok ->
        Conn1 = Conn#{method => Method},
        case Method of
        head ->
            {ok, Conn1};
        trace ->
            {ok, Conn1};
        _ ->
            case send(Conn, Body) of
            ok ->
                {ok, Conn1};
            Other ->
                Other
            end
        end;
    Other ->
        Other
    end.

-spec scheme_to_port(atom()) -> non_neg_integer().
scheme_to_port(http) ->
    80;
scheme_to_port(https) ->
    443.

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
    <<"DELETE">>;
method(trace) ->
    <<"TRACE">>.

-spec path(Path :: binary(), Query :: binary() | undefined) -> binary().
path(Path, <<>>) ->
    Path;
path(Path, Query) ->
    <<Path/binary, $?, Query/binary>>.

-spec connect(Scheme :: scheme(), Host :: string() | binary(), Port :: non_neg_integer(), Options :: list(), Timeout :: timeout()) -> {ok, socket()} | error().
connect(Scheme, Host, Port, Options, Timeout) when is_binary(Host) ->
    connect(Scheme, binary_to_list(Host), Port, Options, Timeout);
connect(http, Host, Port, Options, Timeout) ->
    % Set active=false to prevent early read of packets.
    gen_tcp:connect(Host, Port, [binary, {active, false} | Options], Timeout);
connect(https, Host, Port, Options, Timeout) ->
    ssl:start(),
    ssl:connect(Host, Port, [binary, {active, false} | Options], Timeout).

-spec close(Conn :: connection()) -> ret_ok().
close(#{scheme := http, socket := Socket}) ->
    gen_tcp:close(Socket);
close(#{scheme := https, socket := Socket}) ->
    ssl:close(Socket).

-spec getopts(Conn :: connection(), Options :: proplists:proplist()) -> ret_ok().
getopts(#{scheme := http, socket := Socket}, Options) ->
    inet:getopts(Socket, Options);
getopts(#{scheme := https, socket := Socket}, Options) ->
    ssl:getopts(Socket, Options);
getopts(_Conn, _Options) ->
    {error, bad_connection}.

-spec setopts(Conn :: connection(), Options :: proplists:proplist()) -> ret_ok().
setopts(#{scheme := http, socket := Socket}, Options) ->
    inet:setopts(Socket, Options);
setopts(#{scheme := https, socket := Socket}, Options) ->
    ssl:setopts(Socket, Options);
setopts(_Conn, _Options) ->
    {error, bad_connection}.

-spec send(Conn :: connection(), Data :: binary()) -> ret_ok().
send(_Conn, <<>>) ->
    ok;
send(#{scheme := http, socket := Socket}, Data) ->
    gen_tcp:send(Socket, Data);
send(#{scheme := https, socket := Socket}, Data) ->
    ssl:send(Socket, Data).

-spec recv(Conn :: connection(), Length :: non_neg_integer()) -> ret_data().
recv(Conn, Length) ->
    recv(Conn, Length, ?DEFAULT_TIMEOUT).

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

-spec content_length(Result_Or_Headers :: result() | headers()) -> integer().
content_length(#{headers := Headers}) ->
    content_length(Headers);
content_length(#{content_length := Length}) ->
    binary_to_integer(Length, 10);
content_length(_Hdrs) ->
    0.

-spec body_length(Res :: result()) -> integer().
body_length(Res) ->
    byte_size(maps:get(body, Res)).

-spec has_body(Conn :: connection(), Res :: result()) -> boolean().
has_body(#{method := head}, _Res) ->
    false;
has_body(_Conn, #{status := 204}) ->
    false;
has_body(_Conn, #{status := 304}) ->
    false;
has_body(_Conn, #{status := Status}) when 100 =< Status andalso Status =< 199 ->
    false;
has_body(_Conn, Res) ->
    is_chunked(Res) == false andalso content_length(Res) > 0.

-spec response(Conn :: connection()) -> ret_result().
response(Conn) ->
    response(Conn, ?DEFAULT_TIMEOUT).

-spec response(Conn :: connection(), Timeout :: timeout()) -> ret_result().
response(Conn, Timeout) ->
    ok = setopts(Conn, [{active, once}, {packet, http_bin}]),
    response(Conn, Timeout, #{status => 0, headers => #{}, body => <<>>}).

-spec response(Conn :: connection(), Timeout :: timeout(), Res :: result()) -> ret_result().
response(#{socket := Socket} = Conn, Timeout, Res) ->
    receive
    {ssl_error, Socket, Reason} ->
        close(Conn),
        {error, Reason};
    {ssl_closed, Socket} ->
        % The server write end can close before we've read all the data.
        response(Conn, Timeout, Res);
    {tcp_error, Socket, Reason} ->
        close(Conn),
        {error, Reason};
    {tcp_closed, Socket} ->
        % The server write end can close before we've read all the data.
        response(Conn, Timeout, Res);
    {_Type, Socket, {http_response, _, Status, _}} ->
        % _Type :: http | ssl
        ok = setopts(Conn, [{active, once}]),
        response(Conn, Timeout, Res#{status => Status});
    {_Type, Socket, {http_header, _, Key, _, Value}} ->
        Key1 = field_to_atom(Key),
        Hdrs = maps:get(headers, Res),
        ok = setopts(Conn, [{active, once}]),
        response(Conn, Timeout, Res#{headers => Hdrs#{Key1 => Value}});
    {_Type, Socket, {http_error, Data}}  ->
        % Switching to raw packets can still result in some data.
        Body = maps:get(body, Res, <<>>),
        ok = setopts(Conn, [{active, once}]),
        response(Conn, Timeout, Res#{body => <<Body/binary, Data/binary>>});
    {_Type, Socket, http_eoh} ->
        case has_body(Conn, Res) of
        true ->
            ok = setopts(Conn, [{packet, raw}]),
            case recv(Conn, content_length(Res), Timeout) of
            {ok, Data} ->
                {ok, Res#{body => Data}};
            Other ->
                Other
            end;
        false ->
            % Stop reading before first chunk.
            {ok, Res}
        end;
    Other ->
        {error, Other}
    after Timeout ->
        {error, timeout}
    end.

-spec recv_chunk(Conn :: connection()) -> ret_data().
recv_chunk(Conn) ->
    recv_chunk(Conn, ?DEFAULT_TIMEOUT).

-spec recv_chunk(Conn :: connection(), Timeout :: timeout()) -> ret_data().
recv_chunk(#{socket := Socket} = Conn, Timeout) ->
    ok = setopts(Conn, [{active, once}, {packet, line}]),
    receive
    {ssl_error, Socket, Reason} ->
        close(Conn),
        {error, Reason};
    {ssl_closed, Socket} ->
        close(Conn),
        {ok, <<>>};
    {tcp_error, Socket, Reason} ->
        close(Conn),
        {error, Reason};
    {tcp_closed, Socket} ->
        close(Conn),
        {ok, <<>>};
    {_Type, Socket, Line} ->
        % _Type :: tcp | ssl
        Hex = string:trim(Line),
        Len = binary_to_integer(Hex, 16),
        ok = setopts(Conn, [{packet, raw}]),
        case recv(Conn, Len + 2, Timeout) of
        {ok, Data} ->
            <<Data1:Len/binary, "\r\n">> = Data,
            {ok, Data1};
        Other ->
            Other
        end;
    Other ->
        {error, Other}
    after Timeout ->
        {error, timeout}
    end.

-spec send_chunk(Conn :: connection(), Data :: binary()) -> ret_ok().
send_chunk(Conn, Data) ->
    Hex = integer_to_binary(byte_size(Data), 16),
    case send(Conn, <<Hex/binary, "\r\n">>) of
    ok ->
        send(Conn, <<Data/binary, "\r\n">>);
    Other ->
        Other
    end.
