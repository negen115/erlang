%% @author maximus
%% @doc @todo Module for raising events according schedule.


-module(event).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_next_event/0]).



% get next event for sending
get_next_event() ->
	TableId = table:get_ets_table(),
	FirstKey = ets:first(TableId),
	get_min_time_event(TableId, FirstKey, none, none).


%% ====================================================================
%% Internal functions
%% ====================================================================


% get min time event
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


