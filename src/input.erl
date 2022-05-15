%% @author maximus
%% @doc @todo Add description to input.


-module(input).

% our API
-export([start_link/0, stop_link/0]).
-export([add/4, send_next/0, state/0, delete/1, stop/0]).

% our handlers
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {table_id, min_key, min_time, timer_ref}). 


% Defining our function to start event_gen_server process:

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop_link() ->
	gen_server:stop(event_gen_server).
%% ====================================================================
%% API functions
%% ====================================================================



%%
% Add event
% Time static {{2022,5,8},{0,0,0}} or {{2022,5,8}}
% Time dinamic in seconds - integer
%%

add(TaskId, ProcId, Type, Time) ->
	%% check type of variblesd
	check:check_type_of_value({'string', "TaskId", TaskId}),
	check:check_type_of_value({'integer', "ProcId", ProcId}),
	
	case Type of
		static ->
			check:check_type_of_value({'datetime', "Event Time", Time});
	    dinamic ->
			check:check_type_of_value({'integer', "Event Time", ProcId});
		_ ->
			erlang:error("Time type static or dinamic expected")
	end,

    gen_server:cast(?MODULE, {add, {TaskId, ProcId, Type, Time}}).


send_next() ->
	gen_server:cast(?MODULE, {next}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

% state/0 
% This function will return the current state (here the map who contain all 
% indexed values), we need a synchronous call.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
state() ->
    gen_server:call(?MODULE, {get_state}).

% stop/0
% This function stop cache server process.

stop() ->
    gen_server:stop(?MODULE).



%% ====================================================================
%% Handlers
%% ====================================================================

% init/1
% Here init/1 will initialize state with simple empty map datastructure.

init([]) ->
	% start gen_event
	output:start(),
	
	% get events table
	TableId = table:get_ets_table(),

	% set state statments
	State = #state{table_id = TableId, min_key = none, min_time = none, timer_ref = none},
    {ok, State}.



% handle_cast/2
% put/2 will execute this function.

handle_cast({add, {TaskId, ProcId, Type, Time}}, State) ->

		% get state data
		TableId  = State#state.table_id,
		MinKey   = State#state.min_key,
		MinTime  = State#state.min_time,
		MinTimerRef  = State#state.timer_ref,
		
		
		% get key for event record
		Key = list_to_atom(integer_to_list(ProcId) ++ "-" ++ TaskId),
		
		% convert time
		TimeExec = convert:get_exec_time(Type, Time),
		
		% insert event to table
    	ets:insert(TableId, {Key,TaskId, ProcId, TimeExec}),

		EventKey = event:get_next_event(),
		% get min time r=event data
		[{NewKey, NewTaskId, NewProcId, NewTime}] = ets:lookup(TableId, EventKey),

		
		% get current time for delete invalid old State
		CurTime = convert:get_exec_time(dinamic, 0),
			
		
		if MinTimerRef == none ->
			% create new timer

				{ok, NewTimerRef} = create_timer(NewKey, NewTaskId, NewProcId, NewTime),
		
				NewState = State#state{min_key = NewKey, min_time = NewTime, timer_ref = NewTimerRef};
		   
		    
			MinTime > NewTime orelse MinKey == NewKey orelse MinTime < CurTime ->
			% chenge timer
				timer:cancel(MinTimerRef),

				{ok, NewTimerRef} = create_timer(NewKey, NewTaskId, NewProcId, NewTime),

				NewState = State#state{min_key = NewKey, min_time = NewTime, timer_ref = NewTimerRef};

		   true ->
			   
				NewState = State

		end,

			
    {noreply, NewState};

% Create new timer on next step (called by "output" module when last event is ended)
handle_cast({next}, State) ->
	
		% get state data
		TableId  = State#state.table_id,

		EventKey = event:get_next_event(),

		if EventKey == none ->
			   NewState = State#state{min_key = none, min_time = none, timer_ref = none};
		   true ->
			   [{NewKey, NewTaskId, NewProcId, NewTime}] = ets:lookup(TableId, EventKey),
			   {ok, NewTimerRef} = create_timer(NewKey, NewTaskId, NewProcId, NewTime),
   			   NewState = State#state{min_key = NewKey, min_time = NewTime, timer_ref = NewTimerRef}
		end,
    {noreply, NewState};

% delete/1 will execute this function.

handle_cast({delete, Key}, State) ->
	
		% get state data
		TableId  = State#state.table_id,
		MinKey   = State#state.min_key,
		MinTimerRef  = State#state.timer_ref,
		
		
		% delete record
		ets:delete(TableId, Key),
		
		if MinKey == Key ->
			 	% chenge timer
				timer:cancel(MinTimerRef),
				NewState = State#state{min_key = none, min_time = none, timer_ref = none},
		   		send_next();
		   true ->
				NewState = State
		end,
			 
		
 		%List = ets:tab2list(TableId),
		%io:format("\~-13w => \~p\~n", [del, List]),

		
    {noreply, NewState};


% delete/1 will execute this function.


% All other messages are dropped here.

handle_cast(_Msg, State) ->
    {noreply, State}.



% We don't need other features, other handlers do nothing.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================



create_timer(NewKey, NewTaskId, NewProcId, NewTime) ->

	% get interval for sending event
	Interval = convert:get_time_interval(NewTime),

	% create timer
	timer:apply_after(Interval * 1000, output, send, [NewKey, NewTaskId, NewProcId, NewTime]).





