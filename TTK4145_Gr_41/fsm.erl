-module(fsm).
-export([start/0,state_init/0]).

start() ->
	spawn(fun() -> fsm:state_init() end).


state_init() ->
	receive
		hardware_initialized -> ok
	end,
	
	io:format("State is init~n"),
	elev_driver:set_motor_direction(down),

	receive
		floor_reached -> 
		% Send from driver via scheduler (?)
			state_idle()
	end.

state_idle() ->

	io:format("State is idle~n"),
	elev_driver:set_motor_direction(stop),
	receive
		% Order sent from scheduler
		up ->
			%elev_driver:set_motor_direction(up),
			state_running();
		down ->
			%elev_driver:set_motor_direction(down),
			state_running();
		open ->
			state_doors_open()
	end.

state_running() ->
	io:format("State is running~n"),

	receive
		destination_floor_reached ->
			% Sent from scheduler when a floor_reached event
			% is triggered and the elevator is set to execute
			% an order at that floor.
			state_doors_open()
	end.

state_doors_open() ->
	io:format("State is doors open~n"),
	%elev_driver:set_motor_direction(stop),
	%elev_driver:set_door_open_lamp(on),
	timer:sleep(3000),
	%elev_driver:set_door_open_lamp(off),
	state_idle().