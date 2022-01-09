-module(httpb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("common_test.hrl").

-compile(export_all).

-define(TIMEOUT, 1000).
-define(HELLO, "Hello world.\r\n").

all() ->
    [
        {group, group_basic}
    ].

groups() ->
    [
        {group_basic, [shuffle], basic()}
    ].

basic() ->
    [
        already_closed,
        is_keep_alive,
        has_body,
        connect_fail,
        req_close,
        req_res,
        req_res_timeout,
        req_res_not_found,
        req_res_body,
        req_head_res,
        req_query_res,
        req_options_res,
        req_res_chunks,
        req_res_req_res,
        req_put_res,
        req_delete_res,
        req_post_echo,
        req_post_send_echo,
        req_post_chunk_echo,
        req_post_chunk_fail
    ].

init_per_suite(Config)->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    Config.

end_per_suite(_Config)->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    ok.

init_per_group(Group, [{server, _Pid} | Config]) ->
    init_per_group(Group, Config);

init_per_group(Group = group_basic, Config) ->
    ct:pal(?INFO, "~s:~s ~p", [?MODULE, ?FUNCTION_NAME, Group]),
    {ok, Server} = httpd_dummy:start(),
    [{server, Server} | Config].

end_per_group(Group, Config) ->
    ct:pal(?INFO, "~s:~s ~p", [?MODULE, ?FUNCTION_NAME, Group]),
    Server = proplists:get_value(server, Config),
    ok = httpd_dummy:stop(Server).

already_closed(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Socket} = gen_tcp:connect("localhost", 8008, []),
    ok = gen_tcp:close(Socket),
    ok = httpb:close(#{scheme => http, socket => Socket}).

is_keep_alive(_Config) ->
    true = httpb:is_keep_alive(#{}),
    true = httpb:is_keep_alive(#{connection => <<"other">>}),
    false = httpb:is_keep_alive(#{connection => <<"close">>}),
    false = httpb:is_keep_alive(#{headers => #{connection => <<"close">>}}).

has_body(_Config) ->
    false = httpb:has_body(#{method => head}, #{}),
    false = httpb:has_body(#{}, #{}),
    false = httpb:has_body(#{}, #{status => 204}),
    false = httpb:has_body(#{}, #{status => 304}),
    false = httpb:has_body(#{}, #{status => 199}),
    false = httpb:has_body(#{}, #{transfer_encoding => <<"chunked">>}),
    true  = httpb:has_body(#{}, #{content_length => <<"123">>}),
    false = httpb:has_body(#{}, #{content_length => <<"123">>, transfer_encoding => <<"chunked">>}),
    true  = httpb:has_body(#{}, #{content_length => <<"123">>, transfer_encoding => <<"identity">>}).

connect_fail(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {error, _Reason} = httpb:request(get, "http://localhost", #{}, <<>>).

req_close(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008", #{}, <<>>),
    ok = httpb:close(Conn).

req_res(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008", #{}, <<>>),
    {ok, #{status := 204, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    {ok, [{active, false}]} = httpb:getopts(Conn, [active]),
    ok = httpb:close(Conn).

req_res_timeout(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/bogus", #{}, <<>>),
    {ok, [{active, once}]} = httpb:getopts(Conn, [active]),
    ok = httpb:setopts(Conn, [{active, false}]),
    {error, timeout} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_not_found(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/bogus", #{}, <<>>),
    {ok, #{status := 404}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_body(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/hello", #{}, <<>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn),
    ok = httpb:close(Conn).

req_head_res(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(head, "http://localhost:8008/hello", #{}, <<>>),
    {ok, #{status := 200, headers := Hdrs, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    14 = httpb:content_length(Hdrs),
    ok = httpb:close(Conn).

req_query_res(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(head, "http://localhost:8008/hello?foo=bar", #{}, <<>>),
    {ok, #{status := 200, headers := Hdrs, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    14 = httpb:content_length(Hdrs),
    ok = httpb:close(Conn).

req_options_res(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(options, "http://localhost:8008/hello", #{}, <<>>),
    % httpd does not support OPTIONS yet.
    {ok, #{status := 501}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_chunks(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/chunky", #{}, <<>>),
    {ok, #{status := 200, headers := Headers}} = httpb:response(Conn, ?TIMEOUT),
    true = httpb:is_chunked(Headers),
    {ok, <<"Hello world.\n">>} = httpb:recv_chunk(Conn),
    {ok, <<"Ciao.\n">>} = httpb:recv_chunk(Conn),
    {ok, <<>>} = httpb:recv_chunk(Conn),
    ok = httpb:close(Conn).

req_res_req_res(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/hello", #{}, <<>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    {ok, Conn} = httpb:request(Conn, get, "http://localhost:8008/bogus", #{}, <<>>),
    {ok, #{status := 404}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_put_res(Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    req_method_res(Config, put).

req_delete_res(Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    req_method_res(Config, delete).

req_method_res(_Config, Method) ->
    {ok, Conn} = httpb:request(Method, "http://localhost:8008/", #{}, <<?HELLO>>),
    {ok, #{status := 204, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_post_echo(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(post, "http://localhost:8008/echo", #{
        content_length => integer_to_binary(length(?HELLO)),
        content_type => <<"text/plain">>
    }, <<?HELLO>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_post_send_echo(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(post, "http://localhost:8008/echo", #{
        content_length => integer_to_binary(length(?HELLO)),
        content_type => <<"text/plain">>
    }, <<>>),
    ok = httpb:send(Conn, <<?HELLO>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_post_chunk_echo(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(post, "http://localhost:8008/echo", #{
        transfer_encoding => <<"chunked">>,
        content_type => <<"text/plain">>
    }, <<>>),
    ok = httpb:send_chunk(Conn, <<?HELLO>>),
    ok = httpb:send_chunk(Conn, <<>>),
    {ok, #{status := 200, body := <<?HELLO>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_post_chunk_fail(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(post, "http://localhost:8008/echo", #{
        transfer_encoding => <<"chunked">>,
        content_type => <<"text/plain">>
    }, <<>>),
    ok = httpb:close(Conn),
    {error, closed} = httpb:send_chunk(Conn, <<?HELLO>>).
