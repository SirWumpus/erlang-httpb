%%%
%%% HTTP Basic Client
%%%

-type url()             :: string() | binary().
-type body()            :: binary().
-type headers()         :: #{atom() => binary()}.
-type reason()          :: term().
-type error()           :: {error, reason()}.
-type scheme()          :: http | https.
-type method()          :: delete | get | head | options | post | put | trace.
-type result()          :: #{status => integer(), headers => headers(), body => body(), method => method(), mode => atom()}.
-type socket()          :: gen_tcp:socket() | ssl:sslsocket().
-type options()         :: #{socket_opts => proplists:proplist(), timeout => timeout()}.

-type packet()          :: binary()
                         | {ok, {http_response, _, integer(), _}, binary()}
                         | {ok, {http_header, _, string(), _, binary()}, binary()}
                         | {ok, {http_error, binary()}, binary()}
                         | {ok, http_eoh, binary()}
                         | {more, non_neg_integer() | undefined}
                         | error().

-type state()           :: #{
                            scheme => scheme(),
                            host => binary(),
                            port => non_neg_integer(),
                            socket => socket(),
                            method => method(),
                            carry_over => binary()
                           }.

-type ret_ok()          :: ok | error().
-type ret_conn()        :: {ok, pid()} | error().
-type ret_data()        :: {ok, binary()} | error().
-type ret_result()      :: {ok, result()} | error().

-define(DEFAULT_TIMEOUT, 30000).
