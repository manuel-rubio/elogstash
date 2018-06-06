%% @doc Creates a server to handle a client connection to a TCP logstash
%%      server.
%%      The module is responsible of creating the connection, send the
%%      information to the logstash server and format that information
%%      correctly in a JSON format.
%% @end
-module(elogstash_tcp).
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

-record(state, {
    socket :: gen_tcp:socket(),
    host :: inet:socket_address(),
    port :: inet:port_number()
}).


-spec start_link({inet:socket_address(), inet:port_number()}) -> {ok, pid()}.
%% @doc generates a client to send data to the logstash server.
start_link({Host, Port}) ->
    {ok, _PID} = gen_server:start(?MODULE, {Host, Port}, []).


-spec init({inet:socket_address(), inet:port_number()}) -> {ok, #state{}}.
%% @doc initialize the process to send information data to logstash.
init({Host, Port}) ->
    self() ! connect,
    {ok, #state{
        host = Host,
        port = Port
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
handle_cast(Request, #state{socket = Socket} = State) when is_list(Request)
                                                    orelse is_map(Request) ->
    JSON = elogstash_json:prepare(Request),
    ok = gen_tcp:send(Socket, JSON),
    {noreply, State}.


-spec handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
handle_info(connect, State = #state{socket = undefined,
                                    host = Host, port = Port}) ->
    Opts = [binary, {packet, 0}, {active, once}],
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Socket} ->
            {noreply, State#state{socket = Socket}};
        {error, Reason} ->
            {stop, {connect_error, Reason}, State}
    end;

%% @doc handle raw messages arrived to the process using erlang:send/2 or '!'.
handle_info({tcp, Socket, _Data}, #state{socket = Socket} = State) ->
    %% we shouldn't receive anything via Socket
    {stop, normal, State};

handle_info({tcp_error, _Port, Reason = etimedout}, State) ->
    error_logger:warning_msg("~w: Connection failed due to ~w.",
                             [?MODULE, Reason]),
    {stop, normal, State};

handle_info({tcp_error, _Port, Reason}, State) ->
    error_logger:error_msg("~w: Connection failed due to ~w.",
                           [?MODULE, Reason]),
    {stop, normal, State};

handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {stop, normal, State#state{socket = undefined}}.


-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                          term()),
                State :: term()) -> ok.
%% @doc called when the process finished in a controlled way (using stop in the
%%      previous callbacks).
%% @end
terminate(_Reason, #state{socket = undefined}) ->
    ok;
terminate(_Reason, #state{socket = Socket}) ->
    ok = gen_tcp:close(Socket).


-spec code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                  Extra :: term()) ->
    {ok, NewState :: term()}.
%% @doc called when a hot update is performed.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
