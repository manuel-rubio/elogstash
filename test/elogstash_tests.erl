-module(elogstash_tests).
-author("Manuel Rubio <manuel@altenwald.com>").
-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

suite_test_() ->
    {setup, fun start/0,
            fun stop/1,
            fun tests/1}.

start() ->
    {ok, LSocket} = gen_tcp:listen(0, [{active, true}]),
    {ok, Port} = inet:port(LSocket),
    ok = application:load(elogstash),
    ok = application:set_env(elogstash, connection, {{127,0,0,1}, Port}),
    ok = application:set_env(elogstash, max_workers, 1),
    {ok, _} = elogstash:start(),
    {ok, Socket} = gen_tcp:accept(LSocket, 1000),
    {LSocket, Socket}.

stop({LSocket, _Socket}) ->
    %% Socket was closed during the test
    ok = gen_tcp:close(LSocket),
    ok = application:stop(elogstash),
    ok.

tests({_LSocket, Socket}) ->
    %% testing sending a simple message via proplist:
    ok = elogstash:send([{<<"message">>, <<"hey! ho! let's go!">>}]),
    Data1 = recv(),

    %% breaking...
    ok = gen_tcp:send(Socket, <<"breaking the law!">>),

    %% testing sending a message using maps after restarting worker:
    ok = elogstash:send(#{message => <<"born to be wild!">>}),
    Data2 = recv(),

    ok = gen_tcp:close(Socket),
    timer:sleep(500),

    %% using other parts of the code...
    poolboy:transaction(elogstash_tcp, fun(PID) ->
        PID ! ignored,
        ok = gen_server:cast(PID, ignored),
        ok = gen_server:call(PID, ignored)
    end),
    {ok, state} = elogstash_tcp:code_change(0, state, []),

    [?_assertEqual(<<"hey! ho! let's go!">>, Data1),
     ?_assertEqual(<<"born to be wild!">>, Data2)].

recv() ->
    receive
        {tcp, _Socket, JSON} ->
            RawData = jsx:decode(list_to_binary(JSON)),
            proplists:get_value(<<"message">>, RawData)
    after 1000 -> {error, timeout}
    end.
