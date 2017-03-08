-module(test).

-export([get_order/2,get_optimal_order/1, add/1, asd/0]).
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
	SecondOrder= #orders{direction=up,floor=1},
	ThirdOrder = #orders{direction=down,floor=2},

	Orders = [FirstOrder,SecondOrder,ThirdOrder],

	Elev1Status = #elevatorStatus{direction = 0, lastFloor = 0, state = idle, fsm_PID = 1},
	Elev2Status = #elevatorStatus{direction = 0, lastFloor = 3, state = idle, fsm_PID = 2},
	
	Statuses = [Elev1Status, Elev2Status],

	OptimalOrder = calculate_cost_function(Orders, Statuses, FSM_PID).


calculate_cost_function(CurrentOrders, CurrentStatuses, FSM_PID) ->
	
	lists:foreach(fun(N) -> 
		case FSM_PID == N#orders.elevatorPID of
			true ->
				
			false ->
				ok
		end
		end, CurrentOrders)


	%Pair = Order ++ [Cost],

	CalculatedOrders = loop_shit(CurrentOrders, CurrentStatuses, length(Orders), FSM_PID),

	%lists:sort(CalculatedOrders).
	

loop_shit(Orders, Statuses, 0, FSM_PID) ->
	Orders;
loop_shit(Orders, Statuses, N, FSM_PID) ->
	Order = lists:nth(N, Orders),

	% Calculate TakenCost
	case Order#orders.elevatorPID == undefined of
		true ->
			ok;
		false ->
			throw(order_taken)
	end,

	case Order#orders.direction of
		command ->
			io:format("Command found, SHOULD NOT HAPPEN~n"),
		down ->
			AssignedOrder = Order#orders{elevatorPID = FSM_PID}
	end,

	TotalCost = DistanceCost + DirectionCost + TurnCost + ,

	%?ORDERLIST_HANDLER_PID ! {update_order, AssignedOrder},
	NewOrders = lists:delete(Order,Orders) ++ [AssignedOrder],
	loop_shit(NewOrders,N-1, FSM_PID).

