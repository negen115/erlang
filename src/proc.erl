%% @author maximus
%% @doc @todo Add description to proc.


-module(proc).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_receive_pid/0, get_send_pid/0, loop_receive/0, loop_send/2, get_ets_table/0]).

% get or create receiver process
get_receive_pid() ->
	Process = whereis(event_recive),
	if Process == undefined ->
			Pid = start_receive(),
	   		register(event_recive, Pid),
	   		Pid;
	   true ->
		   Process
	end.

% get or create sender process
get_send_pid() ->
	Process = whereis(event_send),
	if Process == undefined ->
			Pid = start_send(),
	   		register(event_send, Pid),
	   		Pid;
	   true ->
		   Process
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

% create receiver process
start_receive() ->
	spawn(fun() -> loop_receive() end).

% create sender process
start_send() ->
	spawn(fun() -> loop_send(none, none) end).

% event receiver process
loop_receive() ->
  receive
	  % add event
    {TaskId, ProcId, Time} ->

	 	% get events table
		TableId = get_ets_table(),
		
		% get key for event record
		Key = integer_to_list(ProcId) ++ "-" ++ TaskId,
		
    	ets:insert(TableId, {Key,TaskId, ProcId, Time}),

		% get id of send process
		SendPid = proc:get_send_pid(),
		
		% start search prosess for sending
		SendPid ! send,
			
 		%List = ets:tab2list(TableId),
		%io:format("\~-13w => \~p\~n", [set, List]),

    	loop_receive();

	  % delete event
    {TaskId, ProcId} ->
		
		% get events table
		TableId = get_ets_table(),
		
		% get key for event record
		Key = integer_to_list(ProcId) ++ "-" ++ TaskId,
		
		% delete record
		ets:delete(TableId, Key),
		
		% get id of send process
		SendPid = proc:get_send_pid(),
		
		% start search prosess for sending
		SendPid ! {delete, Key},
		
    	loop_receive();
		
    stop ->
    	ok;

    _ ->
    	loop_receive()
  end.

% event sender process
loop_send(TimerKey, TimerRef) ->
  receive
	{Key,TaskId, ProcId, Time} ->
		
		io:format("\~-13w => \~p\~n", [event, {Key,TaskId, ProcId, Time}]),
		
		TableId = get_ets_table(),
		% delete sended event record
		ets:delete(TableId, Key),
		
		% start search prosess for sending
		self() ! send,
		
		loop_send(none, none);
	  
    send ->

		TableId = get_ets_table(),
		% get min time event key
		EventKey = event:get_next_event(),
		% get min time r=event data
		[{Key, TaskId, ProcId, Time}] = ets:lookup(TableId, EventKey),

		% check if timer is ended
		if TimerKey == none ->
			   
				% get interval for sending event
				Interval = convert:get_time_interval(Time),

				%create timer for sending event
				NewTimerRef = erlang:send_after(Interval * 1000, self(), {Key,TaskId, ProcId, Time}),
				
				loop_send(Key, NewTimerRef);
		   
			true ->
				
				% check new event is earler then hendling timer event 
				TimerSec = erlang:read_timer(TimerRef),
				EventSec = convert:get_time_interval(Time) * 1000,

				% apply new timer
				if TimerSec > EventSec orelse TimerKey == Key ->
					   % cancel old timer
					   	erlang:cancel_timer(TimerRef),
						
						% get interval for sending event
						Interval = convert:get_time_interval(Time),
						%create timer for sending event
						NewTimerRef = erlang:send_after(Interval * 1000, self(), {Key,TaskId, ProcId, Time}),
						
						loop_send(Key, NewTimerRef);
				   
					true ->
						
						loop_send(TimerKey, TimerRef)
				
					end
		end;
	{delete, Key} ->
		% cancel timer of deleted event
		if TimerKey == Key ->
			   erlang:cancel_timer(TimerRef),
			   loop_send(none, none);
		   true ->
			   loop_send(TimerKey, TimerRef)
		end;
    stop ->
    	ok;
	  
	  _ -> 
		loop_send(none, none)
  end.

% get or create event table
get_ets_table() ->
	ExistTableId = ets:whereis(event_tab),
	if ExistTableId == undefined -> 
		   ets:new(event_tab, [set, public, named_table]);
	   true ->
		   ExistTableId
	end.