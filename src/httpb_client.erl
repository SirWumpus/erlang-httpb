-module(httpb_client).

-behaviour(gen_server).

-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).

% Exported, indirectly called by handle_call/3.  Do not call directly.
-export([recv/3, send/2, recv_chunk/2, send_chunk/2, request/5, response/2]).

-ifdef(TEST).
-export([expect_body/1]).
-endif.

-include("httpb.hrl").

-type ret_ok_state()        :: {ret_ok(), state()}.
-type ret_pid_state()       :: {{ok, pid()} | error(), state()}.
-type ret_data_state()      :: {ret_data(), state()}.
-type ret_result_state()    :: {ret_result(), state()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Server callbacks.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Url, Options}) ->
    SocketOpts = maps:get(socket_opts, Options, []),
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    UriMap = uri_string:parse(Url),
    Scheme = binary_to_atom(maps:get(scheme, UriMap)),
    Host = maps:get(host, UriMap, <<"localhost">>),
    Port = maps:get(port, UriMap, scheme_to_port(Scheme)),
    case connect(Scheme, Host, Port, SocketOpts, Timeout) of
    {ok, Socket} ->
        State = #{scheme => Scheme, host => Host, port => Port, socket => Socket},
        {ok, State};
    {error, Reason} ->
        {stop, Reason}
    end.

terminate(_Reason, _State) ->
    ok.

handle_call(close, _From, State) ->
    {stop, normal, close(State), #{}};

handle_call({Fun, Args}, _From, State0) ->
    {Result, State1} = erlang:apply(Fun, [State0 | Args]),
    {reply, Result, State1};

handle_call(Request, _From, State) ->
    % Unknown request, no change.
    io:format(standard_error, "~s:~s~p unknown req=~p state=~p~n", [?MODULE, ?FUNCTION_NAME, self(), Request, State]),
    {reply, {error, Request}, State}.

handle_cast(Request, State) ->
    % Unknown request, no change.
    io:format(standard_error, "~s:~s~p unknown req=~p state=~p~n", [?MODULE, ?FUNCTION_NAME, self(), Request, State]),
    {noreply, State}.

handle_info(Info, State) ->
    % Unknown request, no change.
    io:format(standard_error, "~s:~s~p unknown info=~p state=~p~n", [?MODULE, ?FUNCTION_NAME, self(), Info, State]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec scheme_to_port(atom()) -> non_neg_integer().
scheme_to_port(http) ->
    80;
scheme_to_port(https) ->
    443.

-spec method(method()) -> binary().
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

-spec connect(scheme(), string() | binary(), non_neg_integer(), list(), timeout()) -> {ok, socket()} | error().
connect(Scheme, Host, Port, Options, Timeout) when is_binary(Host) ->
    connect(Scheme, binary_to_list(Host), Port, Options, Timeout);
connect(http, Host, Port, Options, Timeout) ->
    % Set active=false to prevent early read of packets.
    gen_tcp:connect(Host, Port, [binary, {active, false} | Options], Timeout);
connect(https, Host, Port, Options, Timeout) ->
    ssl:start(),
    ssl:connect(Host, Port, [binary, {active, false} | Options], Timeout).

-spec setopts(state(), proplists:proplist()) -> ok | error().
setopts(#{scheme := http, socket := Socket}, Options) ->
    inet:setopts(Socket, Options);
setopts(#{scheme := https, socket := Socket}, Options) ->
    ssl:setopts(Socket, Options).

-spec expect_body(result()) -> boolean().
expect_body(#{method := head}) ->
    false;
expect_body(#{status := 204}) ->
    false;
expect_body(#{status := 304}) ->
    false;
expect_body(#{status := Status}) when Status < 200 ->
    false;
expect_body(Res) ->
    httpb:is_chunked(Res) == false andalso httpb:content_length(Res) > 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exported, indirectly called by handle_call/3.  Do not call directly.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec close(state()) -> ret_ok().
close(#{scheme := http, socket := Socket}) ->
    gen_tcp:close(Socket);
close(#{scheme := https, socket := Socket}) ->
    ssl:close(Socket).

-spec recv(state(), non_neg_integer(), timeout()) -> ret_data_state().
recv(#{scheme := http, socket := Socket} = Conn, Length, Timeout) ->
    {gen_tcp:recv(Socket, Length, Timeout), Conn};
recv(#{scheme := https, socket := Socket} = Conn, Length, Timeout) ->
    {ssl:recv(Socket, Length, Timeout), Conn}.

-spec send(state(), binary()) -> ret_ok_state().
send(Conn, <<>>) ->
    {ok, Conn};
send(#{scheme := http, socket := Socket} = Conn, Data) ->
    {gen_tcp:send(Socket, Data), Conn};
send(#{scheme := https, socket := Socket} = Conn, Data) ->
    {ssl:send(Socket, Data), Conn}.

%%%% TODO check Conn.raw

-spec recv_chunk(state(), timeout()) -> ret_data_state().
recv_chunk(#{socket := Socket} = Conn, Timeout) ->
    ok = setopts(Conn, [{active, once}, {packet, line}]),
    receive
    {ssl_error, Socket, Reason} ->
        close(Conn),
        {{error, Reason}, Conn};
    {ssl_closed, Socket} ->
        close(Conn),
        {{ok, <<>>}, Conn};
    {tcp_error, Socket, Reason} ->
        close(Conn),
        {{error, Reason}, Conn};
    {tcp_closed, Socket} ->
        close(Conn),
        {{ok, <<>>}, Conn};
    {_Type, Socket, Line} ->
        % _Type :: tcp | ssl
        Hex = string:trim(Line),
        Len = binary_to_integer(Hex, 16),
        ok = setopts(Conn, [{packet, raw}]),
        case recv(Conn, Len + 2, Timeout) of
        {{ok, Data}, _} ->
            <<Data1:Len/binary, "\r\n">> = Data,
            {{ok, Data1}, Conn};
        Other ->
           {Other, Conn}
        end;
    Other ->
        {Other, Conn}
    after Timeout ->
        {{error, timeout}, Conn}
    end.

-spec send_chunk(state(), binary()) -> ret_ok_state().
send_chunk(Conn, Data) ->
    Hex = integer_to_binary(byte_size(Data), 16),
    case send(Conn, <<Hex/binary, "\r\n">>) of
    {ok, _} ->
        send(Conn, <<Data/binary, "\r\n">>);
    Other ->
        {Other, Conn}
    end.

-spec request(state(), method(), url(), headers(), body()) -> ret_pid_state().
request(#{host := Host, port := Port} = Conn, Method, Url, Hdrs, Body) ->
    UrlMap = uri_string:parse(Url),
%io:format(standard_error, "woot ~p ~p~n", [Url, UrlMap]),
    Path1 = case {Method, maps:get(path, UrlMap)} of
    {options, <<"/*">>} ->
        <<"*">>;
    {options, <<>>} ->
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
    {ok, Conn} ->
        Conn1 = Conn#{method => Method},
        case Method of
        head ->
            {{ok, self()}, Conn1};
        trace ->
            {{ok, self()}, Conn1};
        _ ->
            case send(Conn1, Body) of
            {ok, Conn1} ->
                {{ok, self()}, Conn1};
            Other ->
                {Other, Conn1}
            end
        end;
    Other ->
        {Other, Conn}
    end.

-spec response(state(), timeout()) -> ret_result_state().
response(Conn, Timeout) ->
    % First request or pipelined requests?
    do_data(Conn, Timeout, #{
        status => 0, headers => #{}, body => <<>>,
        mode => status, method => maps:get(method, Conn)
    }, maps:get(carry_over, Conn, <<>>)).

-spec response(state(), timeout(), result()) -> ret_result_state().
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
    {_Transport, Socket, Data} ->
        % _Transport :: tcp | ssl
        CarryOver = maps:get(carry_over, Conn, <<>>),
        do_data(Conn, Timeout, Res, <<CarryOver/binary, Data/binary>>);
    Other ->
        {Other, Conn}
    after Timeout ->
        {{error, timeout}, Conn}
    end.

-spec do_data(state(), timeout(), result(), packet()) -> ret_result_state().
do_data(Conn, Timeout, Res, Data) ->
    case do_packet(Res, Data) of
    {more, Res1, Rest} ->
        setopts(Conn, [{active, once}]),
        response(Conn#{carry_over => Rest}, Timeout, Res1);
    {ok, Res1, Rest} ->
        {{ok, Res1}, Conn#{carry_over => Rest}};
    Other ->
        {Other, Conn}
    end.

-spec do_packet(result(), packet()) ->
    {ok, result(), binary()} | {more, result(), binary()} | error().

do_packet(Res, {ok, {http_response, _, Status, _}, Rest}) ->
    % First (status) line of response; headers next.
    do_packet(Res#{status => Status, mode => headers}, Rest);

do_packet(Res, {ok, {http_header, _, Key, _, Value}, Rest}) ->
    Key1 = field_to_atom(Key),
    Headers = maps:get(headers, Res),
    do_packet(Res#{headers => Headers#{Key1 => Value}}, Rest);

do_packet(Res, {ok, {http_error, Data}, _Rest}) ->
    {error, Res#{carry_over => Data}};

do_packet(Res, {ok, http_eoh, Rest}) ->
    case expect_body(Res) of
    true ->
        do_body(Res, Rest);
    false ->
        % Either response has no body or there are chunks to follow,
        % in which case caller invokes recv_chunk/2 as needed.
        %
        % Poor man's websockets: If the request body was chunked and
        % the response body is chunked then its possible to send a
        % custom server chunked messages and receive chunked messages
        % or replies.  Seen this done is a real application.
        {ok, Res, Rest}
    end;

do_packet(Res, {more, undefined}) ->
    {more, Res, <<>>};

do_packet(#{mode := status} = Res, Data) ->
    % Start of new response, get status line.
    Result = erlang:decode_packet(http_bin, Data, []),
    do_packet(Res, Result);

do_packet(#{mode := headers} = Res, Data) ->
    % Collect response headers.
    Result = erlang:decode_packet(httph_bin, Data, []),
    do_packet(Res, Result);

do_packet(#{mode := body} = Res, Data) ->
    % Collect response body.
    do_body(Res, Data).

-spec do_body(result(), binary()) -> {ok, result(), binary()} | {more, result(), binary()}.
do_body(Res, Data) ->
    % Have we received enough for the response body?
    Length = httpb:content_length(Res),
    case byte_size(Data) < Length of
    true ->
        {more, Res#{mode => body}, Data};
    false ->
        <<Body:Length/binary, Rest/binary>> = Data,
        {ok, Res#{body => Body, mode => done}, Rest}
    end.
