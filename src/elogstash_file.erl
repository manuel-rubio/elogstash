%% @doc Creates a server to handle a file output using JSON for logstash.
%%      The module is responsible of creating the file, rotation, write the
%%      information to the file and format that information correctly in a
%%      JSON format.
%% @end
-module(elogstash_file).
-author("Manuel Rubio <manuel@altenwald.com>").
-compile([warnings_as_errors]).

-behaviour(poolboy_worker).
-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-ifdef(TEST).
-export([get_filename/2]).
-endif.

-include_lib("kernel/include/file.hrl").

-type basename() :: string().
-type rotate() :: monthly | daily | hourly.

-record(state, {
    file :: basename(),
    rotate :: rotate(),
    current_name :: string(),
    handler :: pid()
}).


-spec start_link({basename(), rotate()}) -> {ok, pid()}.
%% @doc generates a client to send data to file.
start_link({Basename, Rotation}) ->
    {ok, _PID} = gen_server:start(?MODULE, {Basename, Rotation}, []).


-spec init({basename(), rotate()}) -> {ok, #state{}}.
%% @doc initialize the process to send information data to the file.
init({Basename, Rotation}) ->
    Filename = get_filename(Basename, Rotation),
    {ok, PID} = file:open(Filename, [append]),
    {ok, #state{
        file = Basename,
        rotate = Rotation,
        current_name = Filename,
        handler = PID
    }}.


-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
%% @doc handle synchronous information sent via gen_server:call/2 function.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
%% @doc handle asynchronous information sent via gen_server:cast/2 function.
handle_cast(Request, #state{file = Basename, rotate = Rotation} = State)
        when is_list(Request) orelse is_map(Request) ->
    NewState = case get_filename(Basename, Rotation) of
        CN when CN =:= State#state.current_name ->
            #state{handler = PID} = State,
            State;
        CN ->
            ok = file:close(State#state.handler),
            {ok, PID} = file:open(CN, [append]),
            ok = file:truncate(PID),
            State#state{current_name = CN, handler = PID}
    end,
    JSON = elogstash_json:prepare(Request),
    ok = file:write(PID, JSON),
    {noreply, NewState}.


-spec handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
%% @doc handle raw messages arrived to the process using erlang:send/2 or '!'.
handle_info(_Info, State) ->
    {stop, normal, State}.


-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                          term()),
                State :: term()) -> ok.
%% @doc called when the process finished in a controlled way (using stop in the
%%      previous callbacks).
%% @end
terminate(_Reason, #state{handler = PID, current_name = Filename}) ->
    error_logger:error_msg("closing file: ~p", [Filename]),
    ok = file:close(PID).


-spec code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                  Extra :: term()) ->
    {ok, NewState :: term()}.
%% @doc called when a hot update is performed.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec get_filename(basename(), rotate()) -> string().
%% @doc generates the filename to be in use by the system.
get_filename(Basename, hourly) ->
    {{Y,M,D}, {H,_,_}} = calendar:universal_time(),
    lists:flatten(io_lib:format("~s-~4..0b-~2..0b-~2..0b-~2..0b.log",
                                [Basename, Y, M, D, H]));

get_filename(Basename, daily) ->
    {{Y,M,D},_} = calendar:universal_time(),
    lists:flatten(io_lib:format("~s-~4..0b-~2..0b-~2..0b.log",
                                [Basename, Y, M, D]));

get_filename(Basename, monthly) ->
    {{Y,M,_},_} = calendar:universal_time(),
    lists:flatten(io_lib:format("~s-~4..0b-~2..0b.log",
                                [Basename, Y, M])).
