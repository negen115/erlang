%% @author maximus
%% @doc @todo Add description to convert.


-module(convert).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_exec_time/2, get_time_interval/1]).

% get time for event sending
get_exec_time(dinamic, Seconds) ->
	LocalTime = calendar:local_time(),
	to_timestamp(LocalTime) + Seconds;

get_exec_time(static, Time) ->
	convert_static_time(Time).

% get interval for event timer
get_time_interval(UnixTime) ->
	LocalTime = calendar:local_time(),
	Interval = UnixTime - to_timestamp(LocalTime),
	if Interval > 0 ->
			Interval;
		true ->
			0
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

% time convertation
convert_static_time({Date}) ->
	to_timestamp({Date, {0,0,0}});

convert_static_time({Date,Time}) ->
	to_timestamp({Date, Time}).


to_timestamp(DateTime) ->
	calendar:datetime_to_gregorian_seconds(DateTime) - epoch_gregorian_seconds().

	
epoch_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).