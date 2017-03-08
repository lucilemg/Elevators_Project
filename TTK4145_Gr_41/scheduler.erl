-module(scheduler).

-export([start/0, statuslist_handler/1, orderlist_handler/1, receive_action/1]).
-include("records.hrl").


start() ->
	%SCHED_LISTENER_PID = spawn (fun() -> scheduler_listener([],[]) end),
	spawn(fun() -> scheduler() end).


scheduler() ->
	io:format("Scheduler ready to receive~n"),
	receive
		{awaiting_orders, FSM_PID} ->
			io:format("Received awaiting_orders ~n"),
			%SCHED_LISTENER_PID ! {update_state, ,Idle,FSM_PID}
			%SCHED_LISTENER_PID ! {orders_wanted, self()},
			NewOrder = receive_action(FSM_PID),
			%FSM_PID ! {execute_order, NewOrder},
			io:format("Scheduler says - FSM, execute: ~p~n",[NewOrder]);
		_ ->
			io:format("received unknown ~n")
	end,
	scheduler().


receive_action(FSM_PID) ->

	CurrentStatuses = get_statuses(),
	OptimalOrder = get_optimal_order(CurrentStatuses, FSM_PID), 

 	CurrentFloor = get_floor(CurrentStatuses, FSM_PID),

	io:format("Current floor: ~p~n",[CurrentFloor]),

	try lists:foreach(fun(N) -> 
		case FSM_PID == N#orders.elevatorPID of
			true ->
				io:format("order found~n"),
				if 
					CurrentFloor < N#orders.floor ->
						throw(order_above);
					CurrentFloor > N#orders.floor ->
						throw(order_below);
					CurrentFloor == N#orders.floor ->
						throw(order_at_floor)
				end;
			false ->
				ok
		end
		end, SortedOrders)

	catch
		throw:order_above ->
			move_up;

		throw:order_below ->
			move_down;

		throw:order_at_floor ->
			open_doors
	end.

get_floor(CurrentStatuses, FSM_PID) ->
	Status = lists:keyfind(FSM_PID,5,CurrentStatuses), 	% Find status of wanted FSM_PID
	Status#elevatorStatus.lastFloor.					% Return floor


get_statuses() ->
	?STATUSLIST_HANDLER_PID ! {get_statuses, self()},
	receive
		{statuses, CurrentStatuses} ->
			ok
	end,
	CurrentStatuses.


statuslist_handler(CurrentStatuses) ->
	%io:format("List of current statuses: ~n~p~n",[CurrentStatuses]),
	receive 
		{update_direction, Direction, ElevPID} ->
			OldStatus = lists:keyfind(ElevPID, 5, CurrentStatuses),
			NewStatus = OldStatus#elevatorStatus{direction = Direction};

		{update_floor, Floor, ElevPID} ->
			OldStatus = lists:keyfind(ElevPID, 5, CurrentStatuses),
			NewStatus = OldStatus#elevatorStatus{lastFloor = Floor};	

		{update_state, State, ElevPID} ->
			OldStatus = lists:keyfind(ElevPID, 5, CurrentStatuses),
			NewStatus = OldStatus#elevatorStatus{state = State};

		{get_statuses, CallerPID} ->
			OldStatus = undefined,
			NewStatus = undefined,
			CallerPID ! {statuses, CurrentStatuses}

	end,
	case NewStatus of
		undefined ->
			statuslist_handler(CurrentStatuses);
		_ ->
			NewStatuses = lists:delete(OldStatus,CurrentStatuses) ++ [NewStatus],
			statuslist_handler(NewStatuses)
	end.


orderlist_handler(CurrentOrders) ->
	io:format("List of current orders: ~n~p~n",[CurrentOrders]),
	receive
		{add_order, NewOrder} ->

			NewOrders = CurrentOrders ++ [NewOrder];

		{remove_order, Floor, FSM_PID} ->

			io:format("Attempting remove of orders at floor ~p~n",[Floor]),
			OldOrderCommand = #orders{direction = command, floor = Floor, elevatorPID = FSM_PID},
			OldOrderUp		= #orders{direction = up, floor = Floor, elevatorPID = FSM_PID},
			OldOrderDown	= #orders{direction = down, floor = Floor, elevatorPID = FSM_PID},
			
			%io:format("order found? : ~p~n",[lists:keyfind(command,2,CurrentOrders)]),

			WithoutCommand = lists:delete(OldOrderCommand, CurrentOrders),
			WithoutUpAndCommand = lists:delete(OldOrderUp, WithoutCommand),
			WithoutUpDownAndCommand = lists:delete(OldOrderDown, WithoutUpAndCommand),

			NewOrders = WithoutUpDownAndCommand;

		{update_order, Order} ->
			% Something happening when orders are reassigned, meaning the cost function 
			% has calculated
			NewOrders = try lists:foreach(fun(N) -> 
				case ((Order#orders.direction == N#orders.direction) and (Order#orders.floor == N#orders.floor)) of
					true ->
						io:format("Found the old order: ~p~n",[N]),
						throw(N);
					false ->
						ok
				end,
				case N == lists:last(CurrentOrders) of
					true ->
						throw(order_not_found);
					_ ->
						ok
				end
				end, CurrentOrders)

				catch
					throw:order_not_found ->
						CurrentOrders;
					throw:N ->
						NewOrder = N#orders{elevatorPID = Order#orders.elevatorPID},
						lists:delete(N,CurrentOrders) ++ [NewOrder]
			end;

		{get_all_orders, CallerPID} ->
			NewOrders = CurrentOrders,
			CallerPID ! {all_orders, CurrentOrders}

	end,
	orderlist_handler(NewOrders).


get_optimal_order(FSM_PID, CurrentStatuses) ->
	%FirstOrder = #orders{direction=1,floor=1,elevatorPID = 1},
	%SecondOrder= #orders{direction=2,floor=3,elevatorPID = TESTER},
	%ThirdOrder = #orders{direction=1,floor=2,elevatorPID = 2},

	?ORDERLIST_HANDLER_PID ! {get_all_orders, self()},
	receive 
		{all_orders, CurrentOrders} ->
			ok
	end,

	OptimalOrder = calculate_cost_function(CurrentOrders, CurrentStatuses, FSM_PID).


calculate_cost_function(CurrentOrders, CurrentStatuses, FSM_PID) ->
	CalculatedOrders = loop_shit(Orders,length(Orders), FSM_PID),
	lists:reverse(CalculatedOrders).


loop_shit(Orders, 0, FSM_PID) ->
	Orders;
loop_shit(Orders, N, FSM_PID) ->
	Order = lists:nth(N, Orders),
	case Order#orders.direction of
		2 ->
			io:format("Command found, assigned to local elevator~n"),
			AssignedOrder = Order#orders{elevatorPID = FSM_PID};
		_ ->
			% calculate best elevator to execute, implement 1 & 2 = up & down
			AssignedOrder = Order#orders{elevatorPID = FSM_PID}
	end,
	?ORDERLIST_HANDLER_PID ! {update_order, AssignedOrder},
	NewOrders = lists:delete(Order,Orders) ++ [AssignedOrder],
	loop_shit(NewOrders,N-1, FSM_PID).



