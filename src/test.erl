%% @author maximus
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([fill_events/0, fill_events_ctatic/0, fill_events_del/0, fill_events_update/0, fill_events_reorder/0]).

% вывод согласно динамического времени
fill_events() ->

    event:add("1", 1, dinamic, 10),
    event:add("2", 1, dinamic, 5),
    event:add("3", 1, dinamic, 20),
    event:add("4", 1, dinamic, 25),
    event:add("5", 1, dinamic, 15).


% вывод согласно статического времени !!! изменить дату и время
fill_events_ctatic() ->

    event:add("1", 1, static, {{2022,5,10},{1,48,10}}),
	event:add("2", 1, static, {{2022,5,10},{1,48,15}}).

% создание и удаление
fill_events_del() ->
    event:add("1", 1, dinamic, 5),
    event:del("1", 1).

%% ====================================================================
%% Internal functions
%% ====================================================================

% изменение существующей записи, в том числе, которая уже считается в таймере

fill_events_update() ->

    event:add("1", 1, dinamic, 10),
    event:add("1", 1, dinamic, 5).


% обавление записи перед той, которая уже считается в таймере

fill_events_reorder() ->

    event:add("1", 1, dinamic, 10),
    event:add("2", 1, dinamic, 5).




