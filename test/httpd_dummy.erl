-module(httpd_dummy).
-behaviour(gen_server).

-export([
	start/0, start/1, start_link/1, stop/1, init/1, terminate/2,
	handle_call/3, handle_cast/2, handle_info/2, do/1, reply_hello/2
]).

-include_lib("inets/include/httpd.hrl").

-spec start() -> any().
start() ->
    start({?MODULE, reply_hello, []}).

-spec start(Args :: list()) -> {ok, pid()} | {error, any()}.
start({_Mod, _Fun, _Args} = MFA) ->
    start([
        {bind_address, {127,0,0,1}},
        {port, 8008},
        {server_root, "."},
        {document_root, "."},
        {dispatch_mfa, MFA}
    ]);

start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

-spec start_link(Args :: list()) -> {ok, pid()} | {error, any()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec stop(Server :: pid()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

-spec init(Args :: list()) -> {ok, any()}.
init(Options) ->
    inets:start(),
    {ok, Server} = inets:start(httpd, Options ++ [
        {profile, ?MODULE},
        {server_name, atom_to_list(?MODULE)},
        {modules, [?MODULE]}
    ], stand_alone),
    Port = proplists:get_value(port, Options),
    io:format("~s~p ready ~p port=~p~n", [?MODULE, self(), Server, Port]),
    {ok, Server}.

-spec terminate(Reason :: term(), State :: any()) -> any().
terminate(_Reason, State) ->
    ok = inets:stop(stand_alone, State).

handle_call(Request, _From, State) ->
    % Unknown request, no change.
    io:format("~s~p unknown call=~p state=~p~n", [?MODULE, self(), Request, State]),
    {noreply, State}.

handle_cast(Request, State) ->
    % Unknown request, no change.
    io:format("~s~p unknown cast=~p state=~p~n", [?MODULE, self(), Request, State]),
    {noreply, State}.

handle_info(Info, State) ->
    % Unknown request, no change.
    io:format("~s~p unknown info=~p state=~p~n", [?MODULE, self(), Info, State]),
    {noreply, State}.

-spec do(Req :: #mod{}) -> tuple().
do(Req = #mod{method = Method, request_uri = Uri, config_db = Config}) ->
    io:format("~s~p REQ ~s ~s~n", [?MODULE, self(), Method, Uri]),
    [{dispatch_mfa, {Module, Function, Args}}] = ets:lookup(Config, dispatch_mfa),
    Module:Function(Req, Args).

-spec reply_hello(Req :: #mod{}, Args :: any()) -> tuple().
reply_hello(Req = #mod{method = Method, request_uri = Uri}, _Args) ->
    {Path, _} = httpd_util:split_path(Uri),
    {Status, Headers, Body} = hello_dispatch(Req, Method, Path),
    io:format("~s~p RES ~s ~s ~B~n", [?MODULE, self(), Method, Uri, Status]),
    {proceed, [{response, {response, [{code, Status}] ++ Headers, Body}}]}.

-define(HELLO, "Hello world.\r\n").

-spec hello_dispatch(Req :: #mod{}, Method :: string(), Path :: string()) ->
    {Status :: integer(), Headers :: list(), Body :: list()}.
hello_dispatch(_Req, _Method, "/") ->
    {204, [], []};
hello_dispatch(_Req, "OPTIONS", "*") ->
    {200, [
        {allow, "GET, HEAD, OPTIONS, POST"},
        {content_type, "text/plain"},
        {content_length, "0"}
    ], []};
hello_dispatch(_Req, "OPTIONS", "/hello") ->
    {200, [
        {allow, "GET, HEAD, POST"},
        {content_type, "text/plain"},
        {content_length, "0"}
    ], []};
hello_dispatch(_Req, "HEAD", "/hello") ->
    {200, [
        {content_type, "text/plain"},
        {content_length, integer_to_list(length(?HELLO))}
    ], []};
hello_dispatch(_Req, "GET", "/hello") ->
    {200, [
        {content_type, "text/plain"},
        {content_length, integer_to_list(length(?HELLO))}
    ], [?HELLO]};
hello_dispatch(_Req, "GET", "/hello/timeout") ->
    {200, [
        {content_type, "text/plain"},
        {content_length, integer_to_list(length(?HELLO))}
    ], []}; % Missing body to force client side timeout.
hello_dispatch(_Req, "GET", "/chunky") ->
    {200, [
        {content_type, "text/plain"},
        {transfer_encoding, "chunked"}
    ], [
        (integer_to_binary(length(?HELLO), 16)), "\r\n", ?HELLO, "\r\n",
        "6\r\nCiao.\n\r\n",
        "0\r\n\r\n"
    ]};
hello_dispatch(#mod{parsed_header = Headers, entity_body = Body}, _Method, "/echo") ->
    ContentType = proplists:get_value("content-type", Headers),
    {200, [
        {content_type, ContentType},
        {content_length, integer_to_list(length(Body))}
    ], Body};
hello_dispatch(_Req, "GET", "/source") ->
    SrcPath = filename:join([filename:dirname(code:which(?MODULE)), "httpd_dummy.erl"]),
    {ok, Source} = file:read_file(SrcPath),
    {200, [
        {content_type, "text/plain"},
        {content_length, integer_to_list(byte_size(Source))}
    ], binary_to_list(Source)};
hello_dispatch(_Req, _Method, _Path) ->
    {404, [], []}.
