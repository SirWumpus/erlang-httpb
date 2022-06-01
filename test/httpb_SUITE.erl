-module(httpb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("common_test.hrl").

-compile(export_all).

-define(TIMEOUT, 1000).
-define(HELLO, "Hello world.\r\n").

all() ->
    [
        {group, group_basic},
        {group, group_ssl}
    ].

groups() ->
    [
        {group_basic, [shuffle], basic()},
        {group_ssl, [shuffle], ssl()}
    ].

basic() ->
    [
        already_closed,
        is_keep_alive,
        bad_connection,
        has_body,
        connect_fail,
        req_close,
        req_res,
        req_res_timeout,
        req_res_not_found,
        req_res_body,
        req_res_body_source,
        req_res_example_com,
        req_head_res,
        req_query_res,
        req_options_res,
        req_options_star_res,
        req_trace_res,
        req_res_chunks,
        req_res_req_res,
        req_req_res_res,
        req_put_res,
        req_delete_res,
        req_post_echo,
        req_post_send_echo,
        req_post_chunk_echo,
        req_post_chunk_fail,
        one_request
    ].

ssl() ->
    [
        ssl_already_closed,
        is_keep_alive,
        bad_connection,
        has_body,
        connect_fail,
        req_close,
        req_res,
        req_res_timeout,
        req_res_not_found,
        req_res_body,
        req_res_body_source,
        req_res_example_com,
        req_head_res,
        req_query_res,
        req_options_res,
        req_options_star_res,
        req_trace_res,
        req_res_chunks,
        req_res_req_res,
        req_req_res_res,
        req_put_res,
        req_delete_res,
        req_post_echo,
        req_post_send_echo,
        req_post_chunk_echo,
        req_post_chunk_fail,
        one_request
    ].

init_per_suite(Config)->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    Config.

end_per_suite(_Config)->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    ok.

init_per_group(Group = group_basic, Config) ->
    ct:pal(?INFO, "~s:~s ~p", [?MODULE, ?FUNCTION_NAME, Group]),
    {ok, Server} = httpd_dummy:start(),
    [{server, Server}, {scheme, "http"}] ++ Config;

% Make a self-sign certificate:
%
%   openssl req -x509 -newkey rsa:4096 -sha256 -days 3650 -nodes \
%       -keyout priv/localhost.key -out priv/localhost.crt -subj "/CN=localhost" \
%       -addext "subjectAltName=DNS:localhost" \
%       -config /usr/share/examples/openssl/openssl.cnf
%
init_per_group(Group = group_ssl, Config) ->
    ct:pal(?INFO, "~s:~s ~p", [?MODULE, ?FUNCTION_NAME, Group]),
    {ok, Server} = httpd_dummy:start([
        {bind_address, {127,0,0,1}},
        {port, 8008},
        {server_root, "."},
        {document_root, "."},
        {dispatch_mfa, {httpd_dummy, reply_hello, []}},
        {socket_type, {essl, [
            {keyfile, filename:join([code:priv_dir(httpb), "localhost.key"])},
            {certfile, filename:join([code:priv_dir(httpb), "localhost.crt"])}
        ]}}
    ]),
    [{server, Server}, {scheme, "https"}] ++ Config.

end_per_group(Group, Config) ->
    ct:pal(?INFO, "~s:~s ~p", [?MODULE, ?FUNCTION_NAME, Group]),
    Server = proplists:get_value(server, Config),
    ok = httpd_dummy:stop(Server).

init_per_testcase(req_res_example_com, _Config) ->
    {skip, external_network};

init_per_testcase(_Test, Config) ->
    Config.

end_per_testcase(_Test, Config) ->
    Config.

already_closed(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Socket} = gen_tcp:connect("localhost", 8008, []),
    ok = gen_tcp:close(Socket),
    ok = httpb:close(#{scheme => http, socket => Socket}).

ssl_already_closed(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Socket} = ssl:connect("localhost", 8008, []),
    ok = ssl:close(Socket),
    ok = httpb:close(#{scheme => https, socket => Socket}).

is_keep_alive(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    true = httpb:is_keep_alive(#{}),
    true = httpb:is_keep_alive(#{connection => <<"other">>}),
    false = httpb:is_keep_alive(#{connection => <<"close">>}),
    false = httpb:is_keep_alive(#{headers => #{connection => <<"close">>}}).

bad_connection(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {error, bad_connection} = httpb:getopts(woot, #{}),
    {error, bad_connection} = httpb:setopts(woot, #{}).

has_body(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    false = httpb:has_body(#{method => head}, #{}),
    false = httpb:has_body(#{}, #{}),
    false = httpb:has_body(#{}, #{status => 204}),
    false = httpb:has_body(#{}, #{status => 304}),
    false = httpb:has_body(#{}, #{status => 199}),
    false = httpb:has_body(#{}, #{transfer_encoding => <<"chunked">>}),
    true  = httpb:has_body(#{}, #{content_length => <<"123">>}),
    false = httpb:has_body(#{}, #{content_length => <<"123">>, transfer_encoding => <<"chunked">>}),
    true  = httpb:has_body(#{}, #{content_length => <<"123">>, transfer_encoding => <<"identity">>}).

connect_fail(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {error, _Reason} = httpb:request(get, Scheme++"://localhost", #{}, <<>>).

req_close(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008", #{}, <<>>),
    ok = httpb:close(Conn).

req_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008", #{}, <<>>),
    {ok, #{status := 204, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    {ok, [{active, false}]} = httpb:getopts(Conn, [active]),
    ok = httpb:close(Conn).

req_res_timeout(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008/hello/timeout", #{}, <<>>),
    {error, timeout} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_not_found(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008/bogus", #{}, <<>>),
    {ok, #{status := 404}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_body(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008/hello", #{}, <<>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn),
    ok = httpb:close(Conn).

req_res_body_source(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008/source", #{}, <<>>),
    {ok, Result} = httpb:response(Conn),
    ok = httpb:close(Conn),
    true = httpb:body_length(Result) =:= httpb:content_length(Result).

req_res_example_com(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://example.com/", #{}, <<>>),
    {ok, [{active, false}]} = httpb:getopts(Conn, [active]),
    {ok, #{status := 200}} = httpb:response(Conn, ?TIMEOUT),
    {ok, [{active, false}]} = httpb:getopts(Conn, [active]),
    ok = httpb:close(Conn).

req_head_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(head, Scheme++"://localhost:8008/hello", #{}, <<>>),
    {ok, #{status := 200, headers := Hdrs, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    14 = httpb:content_length(Hdrs),
    ok = httpb:close(Conn).

req_query_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(head, Scheme++"://localhost:8008/hello?foo=bar", #{}, <<>>),
    {ok, #{status := 200, headers := Hdrs, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    14 = httpb:content_length(Hdrs),
    ok = httpb:close(Conn).

req_options_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(options, Scheme++"://localhost:8008/hello", #{}, <<>>),
    % httpd does not support OPTIONS yet.
    {ok, #{status := 501}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_options_star_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(options, Scheme++"://localhost:8008/*", #{}, <<>>),
    % httpd does not support OPTIONS yet.
    {ok, #{status := 501}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_trace_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(trace, Scheme++"://localhost:8008/", #{}, <<>>),
    % httpd implements TRACE incorrectly, see
    % https://datatracker.ietf.org/doc/html/rfc7231#section-4.3.8
    {ok, #{status := 204, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_chunks(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008/chunky", #{}, <<>>),
    {ok, #{status := 200, headers := Headers}} = httpb:response(Conn, ?TIMEOUT),
    true = httpb:is_chunked(Headers),
    {ok, <<?HELLO>>} = httpb:recv_chunk(Conn),
    {ok, <<"Ciao.\n">>} = httpb:recv_chunk(Conn),
    {ok, <<>>} = httpb:recv_chunk(Conn),
    ok = httpb:close(Conn).

% Synchronised request-response.
req_res_req_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008/hello", #{}, <<>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    {ok, Conn} = httpb:request(Conn, get, Scheme++"://localhost:8008/bogus", #{}, <<>>),
    {ok, #{status := 404}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

% Pipelined requests.
req_req_res_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(get, Scheme++"://localhost:8008/hello", #{}, <<>>),
    {ok, Conn} = httpb:request(Conn, get, Scheme++"://localhost:8008/bogus", #{}, <<>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    {ok, #{status := 404}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_put_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    req_method_res(Config, put).

req_delete_res(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    req_method_res(Config, delete).

req_method_res(Config, Method) ->
    Scheme = proplists:get_value(scheme, Config),
    {ok, Conn} = httpb:request(Method, Scheme++"://localhost:8008/", #{}, <<?HELLO>>),
    {ok, #{status := 204, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_post_echo(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(post, Scheme++"://localhost:8008/echo", #{
        content_length => integer_to_binary(length(?HELLO)),
        content_type => <<"text/plain">>
    }, <<?HELLO>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_post_send_echo(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(post, Scheme++"://localhost:8008/echo", #{
        content_length => integer_to_binary(length(?HELLO)),
        content_type => <<"text/plain">>
    }, <<>>),
    ok = httpb:send(Conn, <<?HELLO>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_post_chunk_echo(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(post, Scheme++"://localhost:8008/echo", #{
        transfer_encoding => <<"chunked">>,
        content_type => <<"text/plain">>
    }, <<>>),
    ok = httpb:send_chunk(Conn, <<?HELLO>>),
    ok = httpb:send_chunk(Conn, <<>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_post_chunk_fail(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, Conn} = httpb:request(post, Scheme++"://localhost:8008/echo", #{
        transfer_encoding => <<"chunked">>,
        content_type => <<"text/plain">>
    }, <<>>),
    ok = httpb:close(Conn),
    {error, closed} = httpb:send_chunk(Conn, <<?HELLO>>).

one_request(Config) ->
    Scheme = proplists:get_value(scheme, Config),
    ct:pal(?INFO, "~s:~s ~s", [?MODULE, ?FUNCTION_NAME, Scheme]),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:one_request(get, Scheme++"://localhost:8008/hello", #{}, <<>>).
