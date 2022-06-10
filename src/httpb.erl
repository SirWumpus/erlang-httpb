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
    request/4, request/5, response/1, response/2, close/1,
    recv_chunk/1, recv_chunk/2, send_chunk/2, recv/2, recv/3, send/2,
    body_length/1, is_chunked/1, is_keep_alive/1, content_length/1,
    one_request/4, one_request/5
]).

-include("httpb.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec close(pid()) -> ret_ok().
close(Pid) ->
    case process_info(Pid, status) of
    undefined ->
        % The process for the connection no longer exists, we're done.
        ok;
    _ ->
        gen_server:call(Pid, close)
    end.

-spec recv(pid(), Length :: non_neg_integer()) -> ret_data().
recv(Pid, Length) ->
    recv(Pid, Length, ?DEFAULT_TIMEOUT).

-spec recv(pid(), Length :: non_neg_integer(), Timeout :: timeout()) -> ret_data().
recv(Pid, Length, Timeout) ->
    gen_server:call(Pid, {fun httpb_client:recv/3, [Length, Timeout]}).

-spec send(pid(), Data :: binary()) -> ret_ok().
send(Pid, Data) ->
    gen_server:call(Pid, {fun httpb_client:send/2, [Data]}).

-spec recv_chunk(pid()) -> ret_data().
recv_chunk(Pid) ->
    recv_chunk(Pid, ?DEFAULT_TIMEOUT).

-spec recv_chunk(pid(), Timeout :: timeout()) -> ret_data().
recv_chunk(Pid, Timeout) ->
    gen_server:call(Pid, {fun httpb_client:recv_chunk/2, [Timeout]}).

-spec send_chunk(pid(), Data :: binary()) -> ret_ok().
send_chunk(Pid, Chunk) ->
    case process_info(Pid, status) of
    undefined ->
        {error, closed};
    _ ->
        gen_server:call(Pid, {fun httpb_client:send_chunk/2, [Chunk]})
    end.

-spec request(Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body()) -> ret_conn().
request(Method, Url, Hdrs, Body) ->
    request(Method, Url, Hdrs, Body, #{socket_opts => []}).

-spec request
    (Pid :: pid(), Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body()) -> ret_conn() ;
    (Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body(), Options :: options()) -> ret_conn() .
request(Method, Url, Hdrs, Body, Options) when is_atom(Method) andalso is_list(Url) ->
    request(Method, list_to_binary(Url), Hdrs, Body, Options);
request(Method, Url, Hdrs, Body, Options) when is_atom(Method) ->
    case gen_server:start(httpb_client, {Url, Options}, []) of
    {ok, Pid} ->
        case request(Pid, Method, Url, Hdrs, Body) of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            % Initial request failed, cleanup connection.
            close(Pid),
            Other
        end;
    Other ->
        Other
    end;
request(Pid, Method, Url, Hdrs, Body) when is_pid(Pid) andalso is_list(Url) ->
    request(Pid, Method, list_to_binary(Url), Hdrs, Body);
request(Pid, Method, Url, Hdrs, Body) when is_pid(Pid) ->
    gen_server:call(Pid, {fun httpb_client:request/5, [Method, Url, Hdrs, Body]});
request(_, _, _, _,_) ->
    {error, badarg}.

-spec response(pid()) -> ret_result().
response(Pid) ->
    response(Pid, ?DEFAULT_TIMEOUT).

-spec response(pid(), timeout()) -> ret_result().
response(Pid, Timeout) ->
    gen_server:call(Pid, {fun httpb_client:response/2, [Timeout]}).

-spec one_request(Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body()) -> ret_result().
one_request(Method, Url, Hdrs, Body) ->
    one_request(Method, Url, Hdrs, Body, #{}).

-spec one_request(Method :: method(), Url :: url(), Hdrs :: headers(), Body :: body(), Options :: options()) -> ret_result().
one_request(Method, Url, Hdrs, Body, Options) ->
    case request(Method, Url, Hdrs#{connection => <<"close">>}, Body, Options) of
    {ok, Pid} ->
        Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
        Result = response(Pid, Timeout),
        close(Pid),
        Result;
    Other ->
        Other
    end.

-spec body_length(Res :: result()) -> integer().
body_length(Res) ->
    byte_size(maps:get(body, Res)).

-spec content_length(Result_Or_Headers :: result() | headers()) -> integer().
content_length(#{headers := Headers}) ->
    content_length(Headers);
content_length(#{content_length := Length}) ->
    binary_to_integer(Length, 10);
content_length(_Hdrs) ->
    0.

-spec is_chunked(Result_Or_Headers :: result() | headers()) -> boolean().
is_chunked(#{headers := Headers}) ->
    is_chunked(Headers);
is_chunked(#{transfer_encoding := <<"chunked">>}) ->
    true;
is_chunked(_Hdrs) ->
    false.

-spec is_keep_alive(Result_Or_Headers :: result() | headers()) -> boolean().
is_keep_alive(#{headers := Headers}) ->
    is_keep_alive(Headers);
is_keep_alive(#{connection := <<"close">>}) ->
    false;
is_keep_alive(_Hdrs) ->
    true.
