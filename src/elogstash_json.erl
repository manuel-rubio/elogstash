%% @doc Handle the JSON transformation and data manipulation.
%% @end
-module(elogstash_json).
-author("Manuel Rubio <manuel@altenwald.com>").
-compile([warnings_as_errors]).

-export([prepare/1]).

-define(DEFAULT_TYPE, <<"erlang_logstash">>).

-spec prepare([proplists:property()] | map()) -> binary().
%% @doc converts a property list or map in a JSON binary string.
prepare(Request) when is_map(Request) ->
    prepare(maps:to_list(Request));

prepare(Request) ->
    Type = application:get_env(elogstash, type, ?DEFAULT_TYPE),
    Fix = [{<<"@timestamp">>, timestamp()},
           {<<"type">>, Type}],
    JSON = jsx:encode(Request ++ Fix),
    <<JSON/binary, "\n">>.


-spec timestamp() -> binary().
%% @doc retrieves the UTC timestamp in YYYY-MM-DDThh:mm:ss.sss format.
timestamp() ->
    Now = {_,_,MicroSecs} = os:timestamp(),
    MilliSecs = MicroSecs div 1000,
    {{Y,M,D}, {H,I,S}} = calendar:now_to_universal_time(Now),
    Str = io_lib:format("~4..0b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b.~3..0b",
                        [Y, M, D, H, I, S, MilliSecs]),
    iolist_to_binary(Str).
