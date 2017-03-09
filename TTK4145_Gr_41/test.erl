-module(test).

-export([get_order/2,get_optimal_order/1, asd/0]).
-include("records.hrl").

%-record (orders, {direction, floor, elevatorPID}).

asd() ->

	Order = #orders{direction = 1, floor = 2, elevatorPID = 10},
	CurrentOrders = get_optimal_order(5),

	NewOrders = try lists:foreach(fun(N) -> 
		case ((Order#orders.direction == N#orders.direction) and (Order#orders.floor == N#orders.floor)) of
		true ->
			io:format("Found the old order: ~p~n",[N]),
			%OldOrder = N,
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
			io:format("Nothing found.. ~n"),
			CurrentOrders;
		throw:N ->
			%NewOrder = OldOrder#orders{elevatorPID = Order#orders.elevatorPID};
			io:format("catch done ~n"),
			NewOrder = N#orders{elevatorPID = Order#orders.elevatorPID},
			lists:delete(N,CurrentOrders) ++ [NewOrder]
	end,
	io:format("New list is: ~p~n",[NewOrders]).


get_order(FSM_PID, CurrentFloor) ->
	

 	%SortedOrders = get_sorted_orders(2),
	%CurrentFloor = get_floor(FSM_PID),

	try lists:foreach(fun(N) -> 
		case FSM_PID == N#orders.elevatorPID of
			true ->
				if 
					CurrentFloor < N#orders.floor ->
						throw(order_up);
					CurrentFloor > N#orders.floor ->
						throw(order_down);
					CurrentFloor == N#orders.floor ->
						throw(open_doors)
				end;
			false ->

				ok
		end
		end, [a,b,c])

	catch
		throw:order_up ->
			up;

		throw:order_down ->
			down;

		throw:open_doors ->
			open
end.

get_optimal_order(FSM_PID) ->

	FirstOrder = #orders{direction=command, floor=1, elevatorPID = 1},
	SecondOrder= #orders{direction=up,floor=2},
	ThirdOrder = #orders{direction=down,floor=2},

	Orders = [FirstOrder,SecondOrder,ThirdOrder],
	%Orders = [],

	Elev1Status = #elevatorStatus{direction = none, lastFloor = 0, state = idle, fsm_PID = 1},
	Elev2Status = #elevatorStatus{direction = up, lastFloor = 2, state = idle, fsm_PID = 2},
	
	Statuses = [Elev1Status, Elev2Status],
	Status = lists:keyfind(FSM_PID,5,Statuses),


	CalculatedOrders = cost_function_loop(Orders, Status, length(Orders)),

	% Sort by lowest cost
	F = fun(X,Y) -> lists:nth(4,X) < lists:nth(4,Y) end,
	SortedList = lists:sort(F,CalculatedOrders),

	try
		case length(SortedList) of 
			0 -> throw(no_orders);
			_ -> throw(order_found)
		end
	catch
		throw:no_orders ->
			no_orders;
		throw:order_found ->
			lists:nth(1,SortedList)
	end.



cost_function_loop(Orders, _, 0) ->
	Orders;
cost_function_loop(Orders, Status, N) ->

	Order = lists:nth(N, Orders),
	OrderAsList  = [Order#orders.direction, Order#orders.floor, Order#orders.elevatorPID],

	% Calculate TakenCost
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


	TotalCost = DirectionCost + DistanceCost,

	CalculatedOrder = OrderAsList ++ [TotalCost],
	NewOrders = lists:delete(Order,Orders) ++ [CalculatedOrder],
	cost_function_loop(NewOrders, Status, N-1).




