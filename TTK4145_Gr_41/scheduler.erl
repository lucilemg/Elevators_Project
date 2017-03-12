-module(scheduler).

-export([start/0, statuslist_handler/1, orderlist_handler/1, receive_action/1, get_all_orders/0, get_statuses/0]).
-include("records.hrl").


start() ->
	%SCHED_LISTENER_PID = spawn (fun() -> scheduler_listener([],[]) end),
	%spawn(fun() -> scheduler() end)
	ok.


receive_action(ElevID) ->
	CurrentStatuses = get_statuses(),
	OptimalOrder = get_optimal_order(CurrentStatuses, ElevID),

	%io:format("Optimal order is now: ~p~n",[OptimalOrder]), 

 	CurrentFloor = get_floor(CurrentStatuses, ElevID),

	try 
	 	case OptimalOrder == no_order of
	 		true 	-> throw(no_order);
	 		_ 		-> ok
	 	end,

		OrderFloor = lists:nth(2,OptimalOrder),
		%io:format("Found order to execute: ~p~n",[OptimalOrder]),

		% Assign order to elevator, notify orderlist
		AssignerOrderRecord = #orders{direction = lists:nth(1,OptimalOrder), floor = lists:nth(2,OptimalOrder), elevatorPID = ElevID},
		?ORDERLIST_HANDLER_PID ! {update_order, AssignerOrderRecord, ?LOCAL},

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

get_floor(CurrentStatuses, ElevID) ->
	Status = lists:keyfind(ElevID,5,CurrentStatuses), 	% Find status of wanted ElevID
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
	%io:format("My statuslist: ~p~n",[CurrentStatuses]),

	%io:format("List of current statuses: ~n~p~n",[CurrentStatuses]),
	receive 
		{update_direction, Direction, ElevPID, SenderType} ->
			OldStatus = lists:keyfind(ElevPID, 5, CurrentStatuses),
			NewStatus = OldStatus#elevatorStatus{direction = Direction};

			%case SenderType of
			%	network -> ok;
			%		local 	-> network:broadcast({update_direction, Direction, ElevPID})
			%end;

		{update_floor, Floor, ElevPID, SenderType} ->
			OldStatus = lists:keyfind(ElevPID, 5, CurrentStatuses),
			NewStatus = OldStatus#elevatorStatus{lastFloor = Floor};

			% case SenderType of
			% 	network -> ok;
			% 	local 	-> network:broadcast({update_floor, Floor, ElevPID})
			% end;

		{update_state, State, ElevPID, SenderType} ->
			OldStatus = lists:keyfind(ElevPID, 5, CurrentStatuses),
			NewStatus = OldStatus#elevatorStatus{state = State};		

			% case OldStatus == NewStatus of
			% 	true -> ok;
			% 	_ -> case SenderType of
			% 			network -> ok;
			% 			local 	-> network:broadcast({update_state, State, ElevPID})
			% 		end
			% end;


		{get_statuses, CallerPID} ->
			OldStatus = undefined,
			NewStatus = undefined,
			CallerPID ! {statuses, CurrentStatuses}


		% {update_all_statuses, Statuses} ->
		% 	OldStatus = undefined,
		% 	NewStatus = Statuses;

		% {remove_status, Status} ->
		% %REMOVE THIS REMOVE; MAKE THINGS BETTER
		% 	OldStatus = undefined,
		% 	NewStatus = lists:delete(Status, CurrentStatuses)

	end,
	case NewStatus of
		undefined ->
			statuslist_handler(CurrentStatuses);
		_ ->
			NewStatuses = lists:delete(OldStatus,CurrentStatuses) ++ [NewStatus],
			statuslist_handler(NewStatuses)
	end.


orderlist_handler(CurrentOrders) ->
	%io:format("~n### ALL CURRENT ORDERS: ~p~n",[CurrentOrders]),
	receive
		{add_order, NewOrder, SenderType} ->

			NewOrders = try case length(CurrentOrders) of
				0 -> throw(order_is_new);
				_ ->
					lists:foreach(fun(N) ->
					
					case ((N#orders.floor == NewOrder#orders.floor) 
						and (N#orders.direction == NewOrder#orders.direction)) of
						true ->
							case N#orders.direction of
								command -> 
									case N#orders.elevatorPID == NewOrder#orders.elevatorPID of
										true ->
											throw(order_is_duplicate);
										_ -> ok
									end;

								_ ->
									throw(order_is_duplicate)
							end;
						_ ->
						 	ok
					end,

					case N == lists:last(CurrentOrders) of
						true ->
							throw(order_is_new);
						_ ->
							ok
					end
					
				end, CurrentOrders)
				end

			catch
				throw:order_is_duplicate ->
					CurrentOrders;

				throw:order_is_new ->
					case SenderType of
						network -> okd;
						local -> network:broadcast({add_order, NewOrder})
					end,
					io:format("Orders after adding new: ~n~p~n",[CurrentOrders ++ [NewOrder]]),
					CurrentOrders ++ [NewOrder]
					%CurrentOrders
			end;		

		{remove_order, Floor, ElevID, SenderType} ->
			io:format("PRE REMOVAL: list of current orders: ~n~p~n",[CurrentOrders]),


			%io:format("Attempting remove of orders at floor ~p~n",[Floor]),
			OldOrderCommand = #orders{direction = command, floor = Floor, elevatorPID = ElevID},
			OldOrderUp		= #orders{direction = up, floor = Floor, elevatorPID = ElevID},
			OldOrderDown	= #orders{direction = down, floor = Floor, elevatorPID = ElevID},
			
			%io:format("order founda? : ~p~n",[lists:keyfind(ElevID,4,CurrentOrders)]),

			WithoutCommand = lists:delete(OldOrderCommand, CurrentOrders),
			WithoutUpAndCommand = lists:delete(OldOrderUp, WithoutCommand),
			WithoutUpDownAndCommand = lists:delete(OldOrderDown, WithoutUpAndCommand),

			NewOrders = WithoutUpDownAndCommand,

			case SenderType of
				network -> ok;
				local -> network:broadcast({remove_order, Floor, ElevID})
			end;


		{update_order, Order, SenderType} ->
			%io:format("Upd: List of current orders: ~n~p~n",[CurrentOrders]),

			% Something happening when orders are reassigned, meaning the cost function 
			% has calculated
			NewOrders = try case length(CurrentOrders) of
				0 ->
					throw(order_not_found);
				_ -> ok
				end,

				lists:foreach(fun(N) -> 
				case ((Order#orders.direction == N#orders.direction) and (Order#orders.floor == N#orders.floor)) of
					true ->
						%io:format("Assinging new elevator to order: ~p~n",[N]),
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
						case SenderType of
							network -> ok;
							local -> network:broadcast({update_order, Order})
						end,
						CurrentOrders;
					throw:N ->
						case SenderType of
							network -> ok;
							local -> network:broadcast({update_order, Order})
						end,
						NewOrder = N#orders{elevatorPID = Order#orders.elevatorPID},
						lists:delete(N,CurrentOrders) ++ [NewOrder]
			end;


		{get_all_orders, CallerPID} ->
			NewOrders = CurrentOrders,
			CallerPID ! {all_orders, CurrentOrders};

		{add_all_orders, Orders} ->
			NewOrders = CurrentOrders ++ Orders;

		{remove_assignments, ElevID} ->

			lists:foreach(fun(N) ->
				case N#orders.elevatorPID of
					ElevID ->
						case N#orders.direction of
							command -> ok;
							_ ->	
								NewOrder = N#orders{elevatorPID = undefined},
								self() ! {update_order, NewOrder, ?NETWORK}
						end;
					_ ->
						ok
				end
			end, CurrentOrders),
			NewOrders = CurrentOrders

	end,
	orderlist_handler(NewOrders).


get_optimal_order(CurrentStatuses, ElevID) ->
	%FirstOrder = #orders{direction=1,floor=1,elevatorPID = 1},
	%SecondOrder= #orders{direction=2,floor=3,elevatorPID = TESTER},
	%ThirdOrder = #orders{direction=1,floor=2,elevatorPID = 2},

	CurrentOrders = get_all_orders(),
	Status = lists:keyfind(ElevID,5,CurrentStatuses),

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
	%io:format("Final list of orders with cost: ~p~n",[Orders]),
	Orders;
cost_function_loop(Orders, Status, N) ->

	Order = lists:nth(N, Orders),
	OrderAsList  = [Order#orders.direction, Order#orders.floor, Order#orders.elevatorPID],

	ElevID = Status#elevatorStatus.fsm_PID,

	try case ((Order#orders.direction == command) and (Order#orders.elevatorPID /= ElevID)) of
		true->
			io:format("Found command that is not for me~n"),
			throw(ignore_order);
		_ ->
			throw(continue)
	end
	catch 
		throw:ignore_order ->
	 		cost_function_loop(lists:delete(Order,Orders),Status,N-1);
	 	throw:continue ->
			case Order#orders.elevatorPID of
				undefined ->
					AvailabilityCost = 1;
				ElevID ->
					AvailabilityCost = 0;
				_ ->
					AvailabilityCost = 20
			end,

			case ((Order#orders.floor == Status#elevatorStatus.lastFloor) and (((Order#orders.direction == up) and (Status#elevatorStatus.direction == down)) 
				or ((Order#orders.direction == down) and (Status#elevatorStatus.direction == up)))) of
				true ->
					DirectionCost = 1;
				false ->
					DirectionCost = 0
			end,

			DistanceCost = abs(Order#orders.floor - Status#elevatorStatus.lastFloor),


			TotalCost = DirectionCost*?NUMBER_OF_FLOORS + DistanceCost*?NUMBER_OF_FLOORS*1.5 + AvailabilityCost,

			CalculatedOrder = OrderAsList ++ [TotalCost],
			NewOrders = lists:delete(Order,Orders) ++ [CalculatedOrder],
			cost_function_loop(NewOrders, Status, N-1)

	end.

	
