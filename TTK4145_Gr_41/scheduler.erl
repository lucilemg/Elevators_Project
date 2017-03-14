-module(scheduler).

-export([init_listhandlers/0, receive_action/1, get_all_orders/0, get_status/0]).
-include("records.hrl").


init_listhandlers() ->

	ElevStatus = #elevatorStatus{direction = down, lastFloor = -1, state = init},
	register(?STATUSLIST_HANDLER_PID, spawn_link (fun() -> statuslist_handler(ElevStatus) end)),
	register(?ORDERLIST_HANDLER_PID,  spawn_link (fun() -> orderlist_handler([]) end)).


receive_action(ElevID) ->
	CurrentStatus = get_status(),
	OptimalOrder = get_optimal_order(CurrentStatus, ElevID),

	try
	 	case OptimalOrder == no_order of
	 		true 	-> throw(no_order);
	 		_ 		-> continue
	 	end,

		OrderFloor 	 = lists:nth(2,OptimalOrder),
		CurrentFloor = CurrentStatus#elevatorStatus.lastFloor,

		%io:format("Found order to execute: ~p~n",[OptimalOrder]),

		% Assign order to elevator, notify orderlist
		AssignerOrderRecord = #orders{ direction 		= lists:nth(1,OptimalOrder), 
									   floor 			= lists:nth(2,OptimalOrder), 
									   assignedElevID 	= ElevID },

		?ORDERLIST_HANDLER_PID ! {reassign_order, AssignerOrderRecord, ?LOCAL},

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
			%io:format("No orders available~n"),
			no_orders_available
	end.


get_status() ->
	?STATUSLIST_HANDLER_PID ! {get_status, self()},
	receive
		{status, CurrentStatus} -> ok
		% Introduce After -> throw error? In case something somehow gets stuck (fault tolerance)
	end,
	CurrentStatus.


get_all_orders() ->
	?ORDERLIST_HANDLER_PID ! {get_all_orders, self()},
	receive
		{all_orders, CurrentOrders} -> ok
	end,
	CurrentOrders.


statuslist_handler(CurrentStatus) ->

	receive

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


orderlist_handler(CurrentOrders) ->
	%io:format("~n### ALL CURRENT ORDERS: ~p~n",[CurrentOrders]),
	receive

		{add_order, NewOrder, SenderType} ->

			NewOrders = try 
				case length(CurrentOrders) of
					0 -> throw(order_is_new);
					_ ->
						% Checks if order is duplicate
						lists:foreach(fun(N) ->
						case   ((N#orders.floor == NewOrder#orders.floor) 
							and (N#orders.direction == NewOrder#orders.direction)) of
							true ->
								case N#orders.direction of
									command -> 

										case N#orders.assignedElevID == NewOrder#orders.assignedElevID of
											true -> throw(order_is_duplicate);
											_    -> ok
										end;

									_ -> throw(order_is_duplicate)

								end;
							_ ->
							 	ok
						end,

						case N == lists:last(CurrentOrders) of
							true -> throw(order_is_new);
							_    ->	ok
						end
					
						end, CurrentOrders)
				end

			catch
				throw:order_is_duplicate ->
					CurrentOrders;

				throw:order_is_new ->
					case SenderType of
						network -> ok;
						local -> network:broadcast({add_order, NewOrder})
					end,
					%io:format("Orders after adding new: ~n~p~n",[CurrentOrders ++ [NewOrder]]),
					CurrentOrders ++ [NewOrder]
			end;		

		{remove_order, Floor, ElevID, SenderType} ->
			%io:format("PRE REMOVAL: list of current orders: ~n~p~n",[CurrentOrders]),
			NewOrders = remove_orders_at_floor(length(CurrentOrders),CurrentOrders,Floor,ElevID),

			case SenderType of
				network -> ok;
				local -> network:broadcast({remove_order, Floor, ElevID})
			end;

		{reassign_order, Order, SenderType} ->
			%io:format("Upd: List of current orders: ~n~p~n",[CurrentOrders]),
			NewOrders = try case length(CurrentOrders) of
				0 -> throw(order_not_found);
				_ -> ok
				end,

				lists:foreach(fun(OldOrder) -> 

				case ((Order#orders.direction == OldOrder#orders.direction) 
					and (Order#orders.floor == OldOrder#orders.floor)) of
					true  -> throw(OldOrder);
					false -> ok
				end,
				case OldOrder == lists:last(CurrentOrders) of
					true  -> throw(order_not_found);
					false -> ok
				end
				end, CurrentOrders)

				catch
					throw:order_not_found ->
						case SenderType of
							network -> ok;
							local -> network:broadcast({reassign_order, Order})
						end,
						CurrentOrders;
					throw:N ->
						case SenderType of
							network -> ok;
							local -> network:broadcast({reassign_order, Order})
						end,
						NewOrder = N#orders{assignedElevID = Order#orders.assignedElevID},
						lists:delete(N,CurrentOrders) ++ [NewOrder]
			end,
			io:format("Reassigned as: ~p~n",[NewOrders]);


		{increment_waiting_time, SenderType} ->

			NewOrders = try 
				case length(CurrentOrders) of
					0 -> throw(no_orders);
					_ -> throw(ok)
				end
				
				catch
					throw:no_orders -> 
						CurrentOrders;
					throw:ok ->
						case SenderType of
							network -> ok;
							local -> network:broadcast({increment_waiting_time})
						end,
						increment_waiting_time(length(CurrentOrders),CurrentOrders)
			end;


		{get_all_orders, CallerPID} ->	

			CallerPID ! {all_orders, CurrentOrders},
			NewOrders = CurrentOrders;


		{add_all_orders, Orders} ->

			lists:foreach(fun(Order) ->
				self() ! {add_order, Order, ?NETWORK},
				io:format("addloop: ~p~n",[Order])
			end, Orders),
			NewOrders = CurrentOrders;


		{remove_assignments, ElevID, SenderType} ->

			% Dissociate orders assigned to unavailable elevator
			lists:foreach(fun(N) ->
				case N#orders.assignedElevID of
					ElevID ->
						case N#orders.direction of
							command -> ok;
							_ ->	
								NewOrder = N#orders{assignedElevID = undefined},
								self() ! {reassign_order, NewOrder, ?NETWORK}
						end;
					_ ->
						ok
				end
			end, CurrentOrders),

			case SenderType of
				network -> ok;
				local 	-> network:broadcast({remove_assignments, ElevID})
			end,
			NewOrders = CurrentOrders

	end,
	orderlist_handler(NewOrders).


get_optimal_order(Status, ElevID) ->

	CurrentOrders = get_all_orders(),

	CalculatedOrders = cost_function_loop(CurrentOrders, Status, ElevID, length(CurrentOrders)),

	% Sorts by lowest cost
	F = fun(X,Y) -> lists:nth(5,X) < lists:nth(5,Y) end,
	SortedList = lists:sort(F,CalculatedOrders),

	try
		case length(SortedList) of 
			0 -> throw(no_order);
			_ -> throw(order_found)
		end
	catch
		throw:no_order 	  -> no_order;
		throw:order_found -> lists:nth(1,SortedList)
	end.


cost_function_loop(Orders, _, _, 0) ->
	%io:format("Orders with costs: ~p~n",[Orders]),
	Orders;
cost_function_loop(Orders, Status, ElevID, N) ->
	
	io:format("STATUS: ~p~n",[Status]),

	Order = lists:nth(N, Orders),
	OrderAsList  = [Order#orders.direction, Order#orders.floor, Order#orders.assignedElevID, ElevID],


	try 
		case ((Order#orders.assignedElevID /= ElevID) 
		  and (Order#orders.assignedElevID /= undefined)) of
		true ->	throw(ignore_order);
		_    ->	throw(calculate_order)
		end
	catch 
		throw:ignore_order ->
	 		cost_function_loop(lists:delete(Order,Orders), Status, ElevID, N-1);

	 	throw:calculate_order ->

	 		WaitTime = Order#orders.waitingTime,

			case Order#orders.assignedElevID of
				undefined ->
					AvailabilityCost = 1;
				_  ->
					AvailabilityCost = 0
			end,

			case   ((Order#orders.direction == up)   and (Status#elevatorStatus.direction == down)
				or ((Order#orders.direction == down) and (Status#elevatorStatus.direction == up))) of
				true ->
					DirectionCost = 1;
				false ->
					DirectionCost = 0
			end,

			DistanceCost = abs(Order#orders.floor - Status#elevatorStatus.lastFloor),


			TotalCost = DistanceCost*?NUMBER_OF_FLOORS*2.5 
					  + DirectionCost*?NUMBER_OF_FLOORS*3 
					  + AvailabilityCost*3 
					  - WaitTime*2,


			CalculatedOrder = OrderAsList ++ [TotalCost],
			NewOrders = lists:delete(Order,Orders) ++ [CalculatedOrder],
			cost_function_loop(NewOrders, Status, ElevID, N-1)

	end.



increment_waiting_time(0, OrderList) ->
	lists:reverse(OrderList);
increment_waiting_time(N, OrderList) ->
	OldOrder = lists:nth(N,OrderList),
	OldWaitingTime = OldOrder#orders.waitingTime,
	NewOrder = OldOrder#orders{waitingTime = OldWaitingTime + 1},

	NewList = lists:delete(OldOrder,OrderList) ++ [NewOrder],
	increment_waiting_time(N-1,NewList).


remove_orders_at_floor(0, OrderList, _, _) ->
	OrderList;
remove_orders_at_floor(N, OrderList, Floor, ElevID) ->
	OldOrder = lists:nth(N,OrderList),
	case   ((OldOrder#orders.floor == Floor) 
	    and (OldOrder#orders.assignedElevID == ElevID)) of
		true -> OrderToRemove = OldOrder;
		false -> OrderToRemove = undefined
	end,
	NewList = lists:delete(OrderToRemove,OrderList),
	remove_orders_at_floor(N-1,NewList,Floor,ElevID).