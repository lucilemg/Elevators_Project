-module(fsm).
-export([start/0]).

%start() ->
%	spawn(fun -> )

state_init() ->
	elev_driver:set_motor_direction(up),
	receive
		floor_reached -> 
			state_idle()
	end.

state_idle() ->
	elev_driver:set_motor_direction(stop),
	receive
		up ->
			elev_driver:set_motor_direction(up),
			state_running();
		down ->
			elev_driver:set_motor_direction(down),
			state_running();
		open ->
			state_doors_open()
	end.

state_running() ->
	receive
		destination_floor_reached ->
			% Sent from scheduler when a floor_reached event
			% is triggered and the elevator is set to execute
			% an order at that floor.
			state_doors_open()
	end.

state_doors_open() ->
	elev_driver:set_motor_direction(stop),
	elev_driver:set_door_open_lamp(on),
	timer:sleep(3000),
	elev_driver:set_door_open_lamp(off),
	state_idle().