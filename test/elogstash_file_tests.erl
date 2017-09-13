-module(elogstash_file_tests).
-author("Manuel Rubio <manuel@altenwald.com>").
-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

daily_suite_test_() ->
    {setup, fun() -> start(daily) end,
            fun stop/1,
            fun tests/1}.

monthly_suite_test_() ->
    {setup, fun() -> start(monthly) end,
            fun stop/1,
            fun tests/1}.

hourly_suite_test_() ->
    {setup, fun() -> start(hourly) end,
            fun stop/1,
            fun tests/1}.

start(Rotate) ->
    File = "/tmp/test_logstash",
    ok = lists:foreach(fun file:delete/1, filelib:wildcard(File ++ "_*")),
    ok = application:load(elogstash),
    ok = application:set_env(elogstash, connection, {file, {File, Rotate}}),
    ok = application:set_env(elogstash, max_workers, 1),
    {ok, _} = elogstash:start(),
    {File, Rotate}.

stop({File, _Rotate}) ->
    ok = application:stop(elogstash),
    ok = application:unload(elogstash),
    ok = lists:foreach(fun file:delete/1, filelib:wildcard(File ++ "_*")),
    ok.

tests({File, Rotate}) ->
    %% testing sending a simple message via proplist:
    ok = elogstash:send([{<<"message">>, <<"hey! ho! let's go!">>}]),
    Data1 = recv(File, Rotate),

    %% force reopen file
    poolboy:transaction(elogstash_pool, fun(PID) ->
        sys:replace_state(PID, fun({state, F, R, _Current, H}) ->
            {state, F, R, "", H}
        end)
    end),

    %% testing sending a message using maps after restarting worker:
    ok = elogstash:send(#{message => <<"born to be wild!">>}),
    Data2 = recv(File, Rotate),

    %% using other parts of the code...
    poolboy:transaction(elogstash_pool, fun(PID) ->
        ok = gen_server:call(PID, ignored),
        PID ! breaking_the_law
    end),
    {ok, state} = elogstash_file:code_change(0, state, []),

    [?_assertEqual(<<"hey! ho! let's go!">>, Data1),
     ?_assertEqual(<<"born to be wild!">>, Data2)].

recv(File, Rotate) ->
    timer:sleep(100),
    CurrentFile = elogstash_file:get_filename(File, Rotate),
    {ok, PID} = file:open(CurrentFile, [binary, read, write]),
    {ok, Line} = file:read_line(PID),
    {ok, 0} = file:position(PID, bof),
    ok = file:truncate(PID),
    ok = file:close(PID),
    [JSON,_] = binary:split(Line, <<"\n">>),
    RawData = jsx:decode(JSON),
    proplists:get_value(<<"message">>, RawData).
