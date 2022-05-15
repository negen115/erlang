%% @author maximus
%% @doc @todo Add description to output.


-module(output).

-compile(export_all).


-behaviour(gen_event).

-record(state, {table_id}). 


%% ====================================================================
%% API
%% ====================================================================

% start gen_event
start() ->
	{ok, _Pid} = gen_event:start_link({local, event_dispatcher}),

	gen_event:add_handler(event_dispatcher, ?MODULE, []).

% send event
send(Key, TaskId, ProcId, TimeExec)->
    gen_event:notify(event_dispatcher, {send, {Key,TaskId, ProcId, TimeExec}}).

% stop gen_event
stop() ->
    ok = gen_event:stop(event_dispatcher),
    stopped.


%% ====================================================================
%% Callbacks
%% ====================================================================


init(_Args) ->
	
	% get events table
	TableId = table:get_ets_table(),
	State = #state{table_id = TableId},
    {ok, State}.


handle_event({send, {Key,TaskId, ProcId, TimeExec}}, State) ->
	    
	io:format("\~-13w => \~p\~n", [handle_event, {Key,TaskId, ProcId, TimeExec}]),
	ets:delete(State#state.table_id, Key),
	input:send_next(),
    {ok, State}.


handle_call(_Other, State)->
    Reply = State,
    {ok, Reply, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================