%% @doc Creates a server to handle a client connection to a UDP logstash
%%      server.
%%      The module is responsible of creating the connection, send the
%%      information to the logstash server and format that information
%%      correctly in a JSON format.
%% @end
-module(elogstash_udp).
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
    socket :: gen_udp:socket(),
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
    Opts = [binary, {active, once}],
    {ok, Socket} = gen_udp:open(0, Opts),
    {ok, #state{
        socket = Socket,
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
handle_cast(Request, #state{socket = Socket, host = Host,
                            port = Port} = State) when is_list(Request)
                                                orelse is_map(Request) ->
    JSON = elogstash_json:prepare(Request),
    ok = gen_udp:send(Socket, Host, Port, JSON),
    {noreply, State}.


-spec handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
%% @doc handle raw messages arrived to the process using erlang:send/2 or '!'.
handle_info({udp, Socket, _IP, _InPortNo, _Packet},
            #state{socket = Socket} = State) ->
    %% we shouldn't receive anything via Socket
    {stop, normal, State}.


-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                          term()),
                State :: term()) -> ok.
%% @doc called when the process finished in a controlled way (using stop in the
%%      previous callbacks).
%% @end
terminate(_Reason, #state{socket = Socket}) ->
    error_logger:error_msg("closing socket: ~p", [Socket]),
    ok = gen_udp:close(Socket).


-spec code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                  Extra :: term()) ->
    {ok, NewState :: term()}.
%% @doc called when a hot update is performed.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
