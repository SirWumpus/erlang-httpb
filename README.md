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
* Options           :: #{socket_opts => list(), timeout => timeout()}.
* Connection        :: #{scheme => http | https, host => string(), port => non_neg_integer(), socket => socket()}.


Exports
-------

### httpb:close(Connection) -> ok | {error, Reason}

Close the `Connection`.

- - -
### httpb:content_length(Headers) -> Length.

Return the content length.

- - -
### httpb:controlling_process(Connection, Pid) -> ok | {error, Reason}

See `gen_tcp:controlling_process/2` or `ssl:controlling_process/2`.

- - -
### httpb:getopts(Connection, Options) -> {ok, Options} | {error, Reason}

See `inet:getopts/2` or `ssl:getopts/2`.

- - -
### httpb:is_chunked(Headers) -> true | false

Return true if `Transfer-Encoding` is present and equal to `chunked`.

- - -
### httpb:is_keep_alive(Headers) -> true | false

Return false if `Connection` header is present and equal to `close`; otherwise true.

- - -
### httpb:recv(Connection, Length) -> {ok, Data} | {error, Reason}
### httpb:recv(Connection, Length, Timeout) -> {ok, Data} | {error, Reason}

Receives a packet from a `Connection` in passive mode (`{active, false}`).  A closed `Connection` is indicated by return value `{error, closed}`.

Argument `Length` is meaningful only when the `Connection` is in mode raw and denotes the number of bytes to read.  If `Length = 0`, all available bytes are returned.  If `Length > 0`, exactly `Length` bytes are returned, or an error; possibly discarding less than `Length` bytes of data when the `Connection` gets closed from the other side.

Optional argument `Timeout` specifies a time-out in milliseconds; default value is 30000.

- - -
### httpb:recv_chunk(Connection) -> {ok, Data} | {error, Reason}
### httpb:recv_chunk(Connection, Timeout) -> {ok, Data} | {error, Reason}

Read the next chunk from the `Connection`.  A zero length `Data` chunk typically indicated end of the response body.

Optional argument `Timeout` specifies a time-out in milliseconds; default value is 30000.

- - -
### httpb:request(Method, Url, Headers, Body) -> {ok, Connection} | {error, Reason}
### httpb:request(Method, Url, Headers, Body, Options) -> {ok, Connection} | {error, Reason}

Start an initial HTTP/1.1 request, returning the `Connection` on success.

- - -
### httpb:request(Connection, Method, Url, Headers, Body) -> {ok, Connection} | {error, Reason}

Given an already open connection, send a request.

- - -
### httpb:response(Connection) -> {ok, Result} | {error, Reason}
### httpb:response(Connection, Timeout) -> {ok, Result} | {error, Reason}

Read the HTTP response line and headers.  If the response is chunked, then no `body` is returned, see `httpb:recv_chunk/1,2`; otherwise read as much of the body as possible.

Optional argument `Timeout` specifies a time-out in milliseconds; default value is 30000.

- - -
### httpb:send(Connection, Data) -> ok | {error, Reason}

Send `Data` to the `Connection`.

- - -
### httpb:send_chunk(Connection, Data) -> ok | {error, Reason}

Send `Data` as an HTTP formatted chunk to the `Connection`.

- - -
### httpb:setopts(Connection, Options) -> ok | {error, Reason}

See `inet:setopts/2` or `ssl:setopts/2`.


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
