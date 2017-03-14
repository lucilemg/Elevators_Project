-module(fsm).
-export([state_init/1]).


state_init(SCHEDULER_PID) ->

	timer:sleep(1000),
	elev_driver:set_motor_direction(down),
	receive
		{stop_at_floor} -> 
			state_idle(SCHEDULER_PID)
	end.


state_idle(SCHEDULER_PID) ->
	elev_driver:set_motor_direction(stop),
	SCHEDULER_PID ! {awaiting_orders},
	receive
		% Order sent from scheduler manager
		{execute_action, move_up} ->
			elev_driver:set_motor_direction(up),
			state_running(SCHEDULER_PID);
		{execute_action, move_down} ->
			elev_driver:set_motor_direction(down),
			state_running(SCHEDULER_PID);
		{execute_action, open_doors} ->
			state_doors_open(SCHEDULER_PID)
	after 1000 ->
		state_idle(SCHEDULER_PID)
	end.


state_running(SCHEDULER_PID) ->
	receive
		{stop_at_floor} ->
			% Sent from scheduler when a floor_reached event
			% is triggered and the elevator is set to execute
			% an order at that floor.
			state_doors_open(SCHEDULER_PID);
		{next_direction, Direction} ->
			elev_driver:set_motor_direction(Direction),
			state_running(SCHEDULER_PID)
	after 10000 ->
			SCHEDULER_PID ! {running_timeout},
			receive 
				{retry} ->
					state_running(SCHEDULER_PID)
			end

	end.

state_doors_open(SCHEDULER_PID) ->
	elev_driver:set_motor_direction(stop),
	elev_driver:set_door_open_lamp(on),
	timer:sleep(3000),
	elev_driver:set_door_open_lamp(off),
	state_idle(SCHEDULER_PID).