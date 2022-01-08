-module(httpd_dummy).
-behaviour(gen_server).

% This should only be used in test suites.
-compile(export_all).

-include_lib("inets/include/httpd.hrl").

-spec start() -> any().
start() ->
    start(204).

-spec start(Args :: list()) -> {ok, pid()} | {error, any()}.
start({_Mod, _Fun, _Args} = MFA) ->
    start([{127,0,0,1}, 8008, ".", ".", MFA]);

start(Status) when is_integer(Status) ->
    start({?MODULE, reply_status, Status});

start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

-spec start_link(Args :: list()) -> {ok, pid()} | {error, any()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec stop(Server :: pid()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

-spec init(Args :: list()) -> {ok, any()}.
init([Bind, Port, SrvRoot, DocRoot, MFA]) ->
    inets:start(),
    {ok, Server} = inets:start(httpd, [
        {profile, ?MODULE},
        {server_name, atom_to_list(?MODULE)},
        {bind_address, Bind},
        {port, Port},
        {server_root, SrvRoot},
        {document_root, DocRoot},
        {dispatch_mfa, MFA},
        {modules, [?MODULE]}
    ], stand_alone),
    io:format("~s~p ready ~p port=~p~n", [?MODULE, self(), Server, Port]),
    {ok, Server}.

-spec terminate(State :: any()) -> any().
terminate(State) ->
    try
        Result = inets:stop(stand_alone, State),
        io:format("~s~p stopped ~p ~p~n", [?MODULE, self(), State, Result])
    catch
        What ->
            io:format("~s~p ~p~n", [?MODULE, self(), What])
    end.

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

-spec reply_status(Req :: #mod{}, Status :: integer()) -> tuple().
reply_status(#mod{method = Method, request_uri = Uri}, Status) ->
    io:format("~s~p RES ~s ~s ~B~n", [?MODULE, self(), Method, Uri, Status]),
    {proceed, [{response, {Status, []}}]}.

-spec reply_content(Req :: #mod{}, {Status :: integer(), Headers :: list(), Body :: list()}) -> tuple().
reply_content(#mod{method = Method, request_uri = Uri}, {Status, Headers, Body}) ->
    io:format("~s~p RES ~s ~s ~B~n", [?MODULE, self(), Method, Uri, Status]),
    {proceed, [{response, {response, [{code, Status}] ++ Headers, Body}}]}.

-spec reply_echo(Req :: #mod{}, Args :: any()) -> tuple().
reply_echo(#mod{method = Method, request_uri = Uri, parsed_header = Headers, entity_body = Body}, _Args) ->
    ContentType = proplists:get_value(content_type, Headers),
    io:format("~s~p RES ~s ~s ~B~n", [?MODULE, self(), Method, Uri, 200]),
    {proceed, [{response, {response, [{code, 200}, {content_type, ContentType}], Body}}]}.

-spec reply_hello(Req :: #mod{}, Args :: any()) -> tuple().
reply_hello(Req = #mod{method = Method, request_uri = Uri}, _Args) ->
    {Path, _} = httpd_util:split_path(Uri),
    {Status, Headers, Body} = hello_dispatch(Req, Method, Path),
    io:format("~s~p RES ~s ~s ~B~n", [?MODULE, self(), Method, Uri, Status]),
io:format("woot ~p~n", [Body]),
    {proceed, [{response, {response, [{code, Status}] ++ Headers, Body}}]}.

-define(HELLO, "Hello world.\r\n").

-spec hello_dispatch(Req :: #mod{}, Method :: string(), Path :: string()) ->
    {Status :: integer(), Headers :: list(), Body :: list()}.
hello_dispatch(_Req, _Method, "/") ->
    {204, [], []};
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
hello_dispatch(_Req, "GET", "/chunky") ->
    {200, [
        {content_type, "text/plain"},
        {transfer_encoding, "chunked"}
    ], [
        "D\r\nHello world.\n\r\n",
        "6\r\nCiao.\n\r\n",
        "0\r\n\r\n"
    ]};
hello_dispatch(#mod{parsed_header = Headers, entity_body = Body}, "POST", "/echo") ->
    ContentType = proplists:get_value("content-type", Headers),
    {200, [
        {content_type, ContentType},
        {content_length, integer_to_list(length(Body))}
    ], Body};
hello_dispatch(_Req, _Method, _Path) ->
    {404, [], ["Not found.\r\n"]}.
