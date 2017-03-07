-module(test).

-export([get_order/2,main/0,get_sorted_orders/0]).
-include("records.hrl").

%-record (orders, {direction, floor, elevatorPID}).


main() ->
	Dasdo = get_order(2,2),
	elev_driver:set_motor_direction(down),
	io:format("Order received: ~p~n",[Dasdo]).





get_order(FSM_PID, CurrentFloor) ->
	

 	SortedOrders = get_sorted_orders(),
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
		end, SortedOrders)

	catch
		throw:order_up ->
			up;

		throw:order_down ->
			down;

		throw:open_doors ->
			open
end.


get_sorted_orders() ->
	FirstOrder = #orders{direction=1,floor=1,elevatorPID = 1},
	SecondOrder= #orders{direction=2,floor=1,elevatorPID = 2},
	ThirdOrder= #orders{direction=1,floor=2,elevatorPID = 2},

	RecordList = [FirstOrder,SecondOrder,ThirdOrder].