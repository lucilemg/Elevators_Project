-module(fsm).
-export([start/1, up/0]).


start(SCHEDULER_PID) ->
	%spawn(fun() -> state_init(SCHEDULER_PID) end).
	state_init(SCHEDULER_PID).


state_init(SCHEDULER_PID) ->
	timer:sleep(1000),

	io:format("State is init~n"),
	elev_driver:set_motor_direction(down),
	io:format("Waiting to receive...~n"),
	receive
		{floor_reached} -> 
			io:format("ye~n"),
			state_idle(SCHEDULER_PID)
	end.

state_idle(SCHEDULER_PID) ->
	elev_driver:set_motor_direction(stop),
	io:format("State is idle~n"),
	SCHEDULER_PID ! {awaiting_orders},
	receive
		% Order sent from scheduler
		{execute_action, move_up} ->
			elev_driver:set_motor_direction(up),
			state_running(SCHEDULER_PID);
		{execute_action, move_down} ->
			elev_driver:set_motor_direction(down),
			state_running(SCHEDULER_PID);
		{execute_action, open_doors} ->
			elev_driver:set_door_open_lamp(on),
			state_doors_open(SCHEDULER_PID)
	after 1000 ->
		state_idle(SCHEDULER_PID)
	end.


state_running(SCHEDULER_PID) ->
	io:format("State is running~n"),

	receive
		{destination_floor_reached} ->
			% Sent from scheduler when a floor_reached event
			% is triggered and the elevator is set to execute
			% an order at that floor.
			state_doors_open(SCHEDULER_PID)
	end.

state_doors_open(SCHEDULER_PID) ->
	io:format("State is doors open~n"),
	elev_driver:set_motor_direction(stop),
	timer:sleep(3000),
	elev_driver:set_door_open_lamp(off),
	state_idle(SCHEDULER_PID).