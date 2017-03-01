-module(fsm).
-export([start/0,state_init/0]).

start(SCHEDULER_PID) ->
	spawn(fun() -> state_init(SCHEDULER_PID) end).


state_init(SCHEDULER_PID) ->
	timer:sleep(1000),

	io:format("State is init~n"),
	elev_driver:set_motor_direction(down),

	receive
		floor_reached -> 
		% Send from driver via scheduler (?)
			state_idle(SCHEDULER_PID)
	end.

state_idle(SCHEDULER_PID) ->

	io:format("State is idle~n"),
	SCHEDULER_PID ! {awaiting_orders, self()},
	elev_driver:set_motor_direction(stop),
	receive
		% Order sent from scheduler
		up ->
			elev_driver:set_motor_direction(up),
			state_running(SCHEDULER_PID);
		down ->
			elev_driver:set_motor_direction(down),
			state_running(SCHEDULER_PID);
		open ->
			state_doors_open(SCHEDULER_PID)
	end.

state_running(SCHEDULER_PID) ->
	io:format("State is running~n"),

	receive
		destination_floor_reached ->
			% Sent from scheduler when a floor_reached event
			% is triggered and the elevator is set to execute
			% an order at that floor.
			state_doors_open(SCHEDULER_PID)
	end.

state_doors_open(SCHEDULER_PID) ->
	io:format("State is doors open~n"),
	elev_driver:set_motor_direction(stop),
	elev_driver:set_door_open_lamp(on),
	timer:sleep(3000),
	elev_driver:set_door_open_lamp(off),
	state_idle(SCHEDULER_PID).