-module(httpb_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("common_test.hrl").

-compile(export_all).

-define(TIMEOUT, 1000).

all() ->
    [
        {group, group_basic},
        {group, group_no_body}
    ].

groups() ->
    [
        {group_basic,   [], basic()},
        {group_no_body, [], [req_res_no_body]}
    ].

basic() ->
    [
        already_closed,
        is_keep_alive,
        req_close,
        req_res,
        req_res_empty,
        req_res_not_found,
        req_res_body,
        req_head_res,
        req_query_res,
        req_options_res,
        req_res_chunks,
        req_res_req_res
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
    {ok, Server} = httpd_dummy:start({httpd_dummy, reply_hello, []}),
    [{server, Server} | Config];

init_per_group(Group = group_no_body, Config) ->
    ct:pal(?INFO, "~s:~s ~p", [?MODULE, ?FUNCTION_NAME, Group]),
    {ok, Server} = httpd_dummy:start(404),
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

req_close(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008", #{}, <<>>),
    ok = httpb:close(Conn).

req_res(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008", #{}, <<>>),
    {ok, #{status := 204}} = httpb:response(Conn, ?TIMEOUT),
    {ok, [{active, _}]} = httpb:getopts(Conn, [active]),
    ok = httpb:close(Conn).

req_res_empty(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/empty", #{}, <<>>),
    {ok, #{status := 204}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_not_found(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/bogus", #{}, <<>>),
    {ok, #{status := 404}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_body(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/hello", #{}, <<>>),
    {ok, #{status := 200, body := <<"Hello world.\r\n">>}} = httpb:response(Conn, ?TIMEOUT),
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
    {ok, <<"Hello world.\n">>} = httpb:recv_chunk(Conn, ?TIMEOUT),
    {ok, <<"Ciao.\n">>} = httpb:recv_chunk(Conn, ?TIMEOUT),
    {ok, <<>>} = httpb:recv_chunk(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_req_res(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/hello", #{}, <<>>),
    {ok, #{status := 200, body := <<"Hello world.\r\n">>}} = httpb:response(Conn, ?TIMEOUT),
    {ok, Conn} = httpb:request(Conn, get, "http://localhost:8008/bogus", #{}, <<>>),
    {ok, #{status := 404}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).

req_res_no_body(_Config) ->
    ct:pal(?INFO, "~s:~s", [?MODULE, ?FUNCTION_NAME]),
    {ok, Conn} = httpb:request(get, "http://localhost:8008/", #{}, <<>>),
    {ok, #{status := 404, body := <<>>}} = httpb:response(Conn, ?TIMEOUT),
    ok = httpb:close(Conn).
