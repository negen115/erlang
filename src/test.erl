%% @author maximus
%% @doc @todo Add description to test.


-module(test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_app/0, fill_events/0, fill_events_static/0, fill_events_del/0, fill_events_update/0, fill_events_reorder/0]).


start_app() ->
	input:start_link().

% вывод согласно динамического времени
fill_events() ->

    input:add("1", 1, dinamic, 4),
    input:add("2", 1, dinamic, 2),
    input:add("3", 1, dinamic, 8),
    input:add("4", 1, dinamic, 10),
    input:add("5", 1, dinamic, 6).


% вывод согласно статического времени !!! изменить дату и время
fill_events_static() ->

    input:add("1", 1, static, {{2022,5,10},{1,48,10}}),
	input:add("2", 1, static, {{2022,5,10},{1,48,15}}).

% создание и удаление
fill_events_del() ->
	input:add("1", 1, dinamic, 8),
    input:add("1", 2, dinamic, 5),
    input:delete("1", 1).

%% ====================================================================
%% Internal functions
%% ====================================================================

% изменение существующей записи, в том числе, которая уже считается в таймере

fill_events_update() ->

    input:add("1", 1, dinamic, 10),
    input:add("1", 1, dinamic, 5).


% обавление записи перед той, которая уже считается в таймере

fill_events_reorder() ->

    input:add("1", 1, dinamic, 10),
    input:add("2", 1, dinamic, 5).




