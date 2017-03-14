-module(test).

-export([asd/0]).
-include("records.hrl").

%-record (orders, {direction, floor, elevatorPID}).


asd() ->
	Stadul = #elevatorStatus{direction = up, lastFloor = 2, state = idle},
	register(?STATUSLIST_HANDLER_PID, spawn (fun() -> statuslist_handler(Stadul) end)),
	Floor = 4,
	timer:sleep(1000),
	?STATUSLIST_HANDLER_PID ! {hello, Floor}.




statuslist_handler(CurrentStatus) ->

	io:format("ready with: ~p~n",[CurrentStatus]),

	receive

		%Anything ->
		%	NewStatus = CurrentStatus,


		{update_floor, Floor} ->
			NewStatus = CurrentStatus#elevatorStatus{lastFloor = Floor};


		{update_direction, Direction} ->
			NewStatus = CurrentStatus#elevatorStatus{direction = Direction};


		{update_state, State} ->
			NewStatus = CurrentStatus#elevatorStatus{state = State};		


		{get_status, CallerPID} ->
			NewStatus = CurrentStatus,
			CallerPID ! {status, CurrentStatus}

	end,
	statuslist_handler(NewStatus).