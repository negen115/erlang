%% @author maximus
%% @doc @todo Module for raising events according schedule.


-module(event).

%% ====================================================================
%% API functions
%% ====================================================================
-export([add/4, del/2, get_next_event/0]).

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
		   

	Pid = proc:get_receive_pid(),


	
	TimeExec = convert:get_exec_time(Type, Time),

	Pid ! {TaskId, ProcId, TimeExec}.


%delete event
del(TaskId, ProcId) ->
	%% check type of varibles
	check:check_type_of_value({'string', "TaskId", TaskId}),
	check:check_type_of_value({'integer', "ProcId", ProcId}),
	
	Pid = proc:get_receive_pid(),

	Pid ! {TaskId, ProcId}.

% get next event for sending
get_next_event() ->
	TableId = proc:get_ets_table(),
	FirstKey = ets:first(TableId),
	get_min_time_event(TableId, FirstKey, none, none).


%% ====================================================================
%% Internal functions
%% ====================================================================


% get nin time event
get_min_time_event(TableId, Key, MinKey, MinTime) ->
    case ets:lookup(TableId, Key) of
        [{Key, _TaskId, _ProcId, Time}] ->
			
			NextKey = ets:next(TableId, Key),
	
			if MinTime < Time ->
					get_min_time_event(TableId, NextKey, MinKey, MinTime);
				true -> 
					get_min_time_event(TableId, NextKey, Key, Time)
			end;
            
		[] ->
			MinKey
	end.


