%% @author maximus
%% @doc @todo Add description to check.


-module(check).

%% ====================================================================
%% API functions
%% ====================================================================
-export([check_type_of_value/1]).



check_type_of_value({'integer', Name, Value}) ->
	if is_integer(Value) ->
			true;
		true -> 
			erlang:error("Parametr " ++ Name ++ " integer expected")
	end;


check_type_of_value({'string', Name, Value}) ->
	IsString = io_lib:printable_list(Value),
	if IsString == true ->
			true;
		true -> 
			erlang:error("Parametr " ++ Name ++ " string expected")
	end;

check_type_of_value({'datetime', _Name, Value}) ->
	valid_datetime(Value).


%% ====================================================================
%% Internal functions
%% ====================================================================



valid_datetime({Date}) ->
	IsDate = calendar:valid_date(Date),
	if IsDate == true ->
			true;
		true -> 
			erlang:error("Parametr Date not in {{Y,M,D}} format")
	end;

valid_datetime({Date,Time}) ->
	IsDateTime = calendar:valid_date(Date) andalso valid_time(Time),
	if IsDateTime == true ->
			true;
		true -> 
			erlang:error("Parametr DateTime not in {{Y,M,D},{H,Min,S}} format")
	end.

valid_time({H,M,S})  when H >= 0, H < 24,
	M >= 0, M < 60,
	S >= 0, S < 60 -> true;
	
valid_time({_,_,_}) -> false.


