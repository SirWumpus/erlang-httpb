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
-type connection()      :: map().

-define(DEFAULT_TIMEOUT, 30000).
