-module(elogstash_app).
-author("Manuel Rubio <manuel@altenwald.com>").
-compile([warnings_as_errors]).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).
-export([init/1]).

-define(DEFAULT_MAX_WORKERS, 10).
-define(DEFAULT_CONNECTION, {"localhost", 5000}).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    Size = application:get_env(elogstash, max_workers, ?DEFAULT_MAX_WORKERS),
    Conn = application:get_env(elogstash, connection, ?DEFAULT_CONNECTION),
    {ok, {one_for_one, 10, 1}, [
        %% TODO create a configuration and a way to let to configure TCP and
        %%      UDP clients.
        child_spec(Size, Conn)
    ]}.

child_spec(Size, Conn) ->
    Name = logstash_tcp,
    poolboy:child_spec(Name, [{name, {local, Name}},
                              {worker_module, elogstash_tcp},
                              {size, Size},
                              {max_overflow, 2}], Conn).
