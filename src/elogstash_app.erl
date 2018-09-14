-module(elogstash_app).
-author("Manuel Rubio <manuel@altenwald.com>").
-compile([warnings_as_errors]).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).
-export([init/1]).

-define(DEFAULT_MAX_WORKERS, 10).
-define(DEFAULT_CONNECTION, {tcp, {"localhost", 5000}}).
-define(POOLNAME, elogstash_pool).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok = poolboy:stop(?POOLNAME).

init([]) ->
    Size = application:get_env(elogstash, max_workers, ?DEFAULT_MAX_WORKERS),
    {Transport, Data} = application:get_env(elogstash, connection,
                                            ?DEFAULT_CONNECTION),
    {ok, {{one_for_one, 10, 1}, [
        child_spec(Size, Transport, Data)
    ]}}.

child_spec(_Size, file, Conn) ->
    Name = ?POOLNAME,
    poolboy:child_spec(Name, [{name, {local, Name}},
                              {worker_module, elogstash_file},
                              {size, 1},
                              {max_overflow, 1}], Conn);

child_spec(Size, tcp, Conn) ->
    Name = ?POOLNAME,
    Overflow = max(Size div 2, 1),
    poolboy:child_spec(Name, [{name, {local, Name}},
                              {worker_module, elogstash_tcp},
                              {size, Size},
                              {max_overflow, Overflow}], Conn);

child_spec(Size, udp, Conn) ->
    Name = ?POOLNAME,
    Overflow = max(Size div 2, 1),
    poolboy:child_spec(Name, [{name, {local, Name}},
                              {worker_module, elogstash_udp},
                              {size, Size},
                              {max_overflow, Overflow}], Conn);

child_spec(Size, http, Conn) ->
    Name = ?POOLNAME,
    Overflow = max(Size div 2, 1),
    poolboy:child_spec(Name, [{name, {local, Name}},
                              {worker_module, elogstash_http},
                              {size, Size},
                              {max_overflow, Overflow}], Conn).
