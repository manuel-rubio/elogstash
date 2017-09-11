%% @doc Functions to send information to logstash. This module let you to
%%      send information to logstash using poolboy to auto-select a free
%%      client from the pool.
%% @end
-module(elogstash).
-author("Manuel Rubio <manuel@altenwald.com>").
-compile([warnings_as_errors]).

-export([
    start/0,
    send/1
]).


-spec start() -> {ok, [atom()]}.
%% @doc shortcut to start the application with its dependencies.
start() ->
    {ok, _} = application:ensure_all_started(elogstash).


-spec send(Info :: [proplists:property()]) -> ok.
%% @doc send information to logstash using a poolboy transaction.
send(Info) ->
    poolboy:transaction(elogstash_tcp, fun(PID) ->
        ok = elogstash_tcp:send(PID, Info)
    end).
