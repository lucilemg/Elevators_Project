-module(scheduler).

-export([start/0, statuslist_handler/1, orderlist_handler/1, receive_action/1, get_all_orders/0]).
-include("records.hrl").


start() ->
	%SCHED_LISTENER_PID = spawn (fun() -> scheduler_listener([],[]) end),
	spawn(fun() -> scheduler() end).


scheduler() ->
	% Either remove this, or make all communication from main go through this thing.
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
	%io:format("Current statuses: ~p~n",[CurrentStatuses]),
	OptimalOrder = get_optimal_order(CurrentStatuses, FSM_PID),

	%io:format("Optimal order is now: ~p~n",[OptimalOrder]), 

 	CurrentFloor = get_floor(CurrentStatuses, FSM_PID),

	try 
	 	case OptimalOrder == no_order of
	 		true 	-> throw(no_order);
	 		_ 		-> ok
	 	end,

		OrderFloor = lists:nth(2,OptimalOrder),

		% Assign order to elevator, notify orderlist
		AssignerOrderRecord = #orders{direction = lists:nth(1,OptimalOrder), floor = lists:nth(2,OptimalOrder), elevatorPID = FSM_PID},
		?ORDERLIST_HANDLER_PID ! {update_order, AssignerOrderRecord},

		if
		CurrentFloor < OrderFloor -> throw(order_above);
		CurrentFloor > OrderFloor -> throw(order_below);
		CurrentFloor == OrderFloor ->throw(order_at_floor)
		end

	catch
		throw:order_above ->
			io:format("Move up~n"),
			move_up;

		throw:order_below ->
			io:format("Move down~n"),
			move_down;

		throw:order_at_floor ->
			io:format("Open doors~n"),
			open_doors;

		throw:no_order ->
			io:format("No orders available~n"),
			no_orders_available
	end.

get_floor(CurrentStatuses, FSM_PID) ->
	Status = lists:keyfind(FSM_PID,5,CurrentStatuses), 	% Find status of wanted FSM_PID
	Status#elevatorStatus.lastFloor.					% Return floor


get_statuses() ->
	?STATUSLIST_HANDLER_PID ! {get_statuses, self()},
	receive
		{statuses, CurrentStatuses} -> ok
		% Introduce After -> throw error? In case something somehow gets stuck (fault tolerance)
	end,
	CurrentStatuses.


get_all_orders() ->
	?ORDERLIST_HANDLER_PID ! {get_all_orders, self()},
	receive
		{all_orders, CurrentOrders} -> ok
	end,
	CurrentOrders.


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

	receive
		{add_order, NewOrder} ->

			NewOrders = CurrentOrders ++ [NewOrder],
			io:format("List of current orders: ~n~p~n",[NewOrders]);


		{remove_order, FSM_PID} ->
			io:format("List of current orders: ~n~p~n",[CurrentOrders]),


			Status = lists:keyfind(FSM_PID,5,get_statuses()),
			Floor = Status#elevatorStatus.lastFloor,

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
			io:format("List of current orders: ~n~p~n",[CurrentOrders]),

			% Something happening when orders are reassigned, meaning the cost function 
			% has calculated
			NewOrders = try lists:foreach(fun(N) -> 
				case ((Order#orders.direction == N#orders.direction) and (Order#orders.floor == N#orders.floor)) of
					true ->
						io:format("Assinging new elevator to order: ~p~n",[N]),
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


get_optimal_order(CurrentStatuses, FSM_PID) ->
	%FirstOrder = #orders{direction=1,floor=1,elevatorPID = 1},
	%SecondOrder= #orders{direction=2,floor=3,elevatorPID = TESTER},
	%ThirdOrder = #orders{direction=1,floor=2,elevatorPID = 2},

	CurrentOrders = get_all_orders(),

	Status = lists:keyfind(FSM_PID,5,CurrentStatuses),

	CalculatedOrders = cost_function_loop(CurrentOrders, Status, length(CurrentOrders)),


	% Sort by lowest cost
	F = fun(X,Y) -> lists:nth(4,X) < lists:nth(4,Y) end,
	SortedList = lists:sort(F,CalculatedOrders),

	try
		case length(SortedList) of 
			0 -> throw(no_order);
			_ -> throw(order_found)
		end
	catch
		throw:no_order ->
			no_order;
		throw:order_found ->
			lists:nth(1,SortedList)
	end.


cost_function_loop(Orders, _, 0) ->
	Orders;
cost_function_loop(Orders, Status, N) ->

	Order = lists:nth(N, Orders),
	OrderAsList  = [Order#orders.direction, Order#orders.floor, Order#orders.elevatorPID],

	% Calculate TakenCost
	% chacnge to availabilityCost, where assigned to self is cheapest, undefined is free (medium cheap), other (taken) is infty (throw none?)
	case Order#orders.elevatorPID == undefined of
		true ->
			ok;
		false ->
			%throw(order_taken)
			ok
	end,

	case Order#orders.direction of
		command ->
			io:format("Command found, SHOULD NOT HAPPEN~n");
			%throw(unassigned_command);
		_ ->
			ok
	end,

	case ((Order#orders.floor == Status#elevatorStatus.lastFloor) and (((Order#orders.direction == up) and (Status#elevatorStatus.direction == down)) 
		or ((Order#orders.direction == down) and (Status#elevatorStatus.direction == up)))) of
		true ->
			DirectionCost = 1;
		false ->
			DirectionCost = 0
	end,

	DistanceCost = abs(Order#orders.floor - Status#elevatorStatus.lastFloor),


	TotalCost = DirectionCost*?NUMBER_OF_FLOORS + DistanceCost*6,

	CalculatedOrder = OrderAsList ++ [TotalCost],
	NewOrders = lists:delete(Order,Orders) ++ [CalculatedOrder],
	cost_function_loop(NewOrders, Status, N-1).

