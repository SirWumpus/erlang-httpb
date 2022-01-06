httpb
=====

An HTTP Basic client that provides finer granularity over the handling of HTTP requests and responses.


Data Types
----------

* Url               :: string() | binary().
* Body              :: binary().
* Data              :: binary().
* Headers           :: map().
* Reason            :: term().
* Method            :: get | head | options | post | put | delete.
* Result            :: #{status => integer(), headers => headers(), body => body()}.
* Socket            :: gen_tcp:socket() | ssl:sslsocket().
* Connection        :: #{scheme => http | https, host => string(), port => non_neg_integer(), socket => socket()}.


Exports
-------

### httpb:close(Connection) -> ok | {error, Reason}

- - -
### httpd:controlling_process(Connection, Pid) -> ok | {error, Reason}

- - -
### httpb:getopts(Connection, Options) -> {ok, Options} | {error, Reason}

- - -
### httpb:is_chunked(Headers) -> true | false

- - -
### httpb:is_keep_alive(Headers) -> true | false

- - -
### httpb:recv(Connection, Length) -> {ok, Data} | {error, Reason}
### httpb:recv(Connection, Length, Timeout) -> {ok, Data} | {error, Reason}

- - -
### httpb:recv_chunk(Connection) -> {ok, Data} | {error, Reason}
### httpb:recv_chunk(Connection, Timeout) -> {ok, Data} | {error, Reason}

- - -
### httpb:request(Method, Url, Headers, Body) -> {ok, Connection} | {error, Reason}
### httpb:request(Connection, Method, Url, Headers, Body) -> {ok, Connection} | {error, Reason}

- - -
### httpb:response(Connection) -> {ok, Result} | {error, Reason}
### httpb:response(Connection, Timeout) -> {ok, Result} | {error, Reason}

Read the HTTP response line and headers.  If the response is chunked, then no `body` is returned, see `httpb:recv_chunk/1,2`; otherwise read as much of the body as possible.

- - -
### httpb:send(Connection, Data) -> ok | {error, Reason}

- - -
### httpb:send_chunk(Connection, Data) -> ok | {error, Reason}

- - -
### httpb:setopts(Connection, Options) -> ok | {error, Reason}


Examples
--------

```
1> {ok, C} = httpb:request(get, "https://example.com/", #{}, <<>>).
{ok,#{host => <<"example.com">>,port => 443,scheme => https,
      socket =>
          {sslsocket,{gen_tcp,#Port<0.5>,tls_connection,undefined},
                     [<0.113.0>,<0.112.0>]}}}
2> {ok, R} = httpb:response(C).
{ok,#{body =>
          <<"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n ... </html>\n">>,
      headers =>
          #{age => <<"408052">>,cache_control => <<"max-age=604800">>,
            content_length => <<"1256">>,
            content_type => <<"text/html; charset=UTF-8">>,
            date => <<"Thu, 06 Jan 2022 17:48:22 GMT">>,
            etag => <<"\"3147526947+gzip+ident\"">>,
            expires => <<"Thu, 13 Jan 2022 17:48:22 GMT">>,
            last_modified => <<"Thu, 17 Oct 2019 07:18:26 GMT">>,
            server => <<"ECS (chb/0286)">>,
            vary => <<"Accept-Encoding">>,x_cache => <<"HIT">>},
      status => 200}}
3> httpb:close(C).
ok
```


Copyright
---------

Copyright 2021 by Anthony Howe.  All rights reserved.


MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
