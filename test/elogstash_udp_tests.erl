-module(elogstash_udp_tests).
-author("Manuel Rubio <manuel@altenwald.com>").
-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

suite_test_() ->
    {setup, fun start/0,
            fun stop/1,
            fun tests/1}.

start() ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
    {ok, Port} = inet:port(Socket),
    Host = {127,0,0,1},
    ok = application:load(elogstash),
    ok = application:set_env(elogstash, connection, {udp, {Host, Port}}),
    ok = application:set_env(elogstash, max_workers, 1),
    {ok, _} = elogstash:start(),
    {Socket, Host, Port}.

stop({Socket, _Host, _Port}) ->
    ok = gen_udp:close(Socket),
    ok = application:stop(elogstash),
    ok = application:unload(elogstash),
    ok.

tests({Socket, Host, _Port}) ->
    %% testing sending a simple message via proplist:
    ok = elogstash:send([{<<"message">>, <<"hey! ho! let's go!">>}]),
    {RemotePort, Data1} = recv(),

    %% breaking...
    ok = gen_udp:send(Socket, Host, RemotePort, <<"breaking the law!">>),
    timer:sleep(500),

    %% testing sending a message using maps after restarting worker:
    ok = elogstash:send(#{message => <<"born to be wild!">>}),
    {_, Data2} = recv(),

    %% using other parts of the code...
    poolboy:transaction(elogstash_pool, fun(PID) ->
        ok = gen_server:call(PID, ignored)
    end),
    {ok, state} = elogstash_udp:code_change(0, state, []),

    [?_assertEqual(<<"hey! ho! let's go!">>, Data1),
     ?_assertEqual(<<"born to be wild!">>, Data2)].

recv() ->
    receive
        {udp, _Socket, _IP, Port, JSON} ->
            RawData = jsx:decode(JSON),
            {Port, proplists:get_value(<<"message">>, RawData)}
    after 1000 -> {error, timeout}
    end.
