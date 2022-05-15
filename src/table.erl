%% @author maximus
%% @doc @todo Add description to table.


-module(table).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_ets_table/0]).


% get or create event table
get_ets_table() ->
	ExistTableId = ets:whereis(event_tab),
	if ExistTableId == undefined -> 
		   ets:new(event_tab, [set, public, named_table]);
	   true ->
		   event_tab
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================


