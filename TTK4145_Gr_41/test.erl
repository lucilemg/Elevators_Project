-module(test).

-export([get/2]).
-include("records.hrl").

%-record (orders, {direction, floor, elevatorPID}).

%get_c(#orders{clients = C, dbname = _D}) ->
%     C.

get(FSM_PID, CurrentFloor) ->
	FirstOrder = #orders{direction=1,floor=1,elevatorPID = 1},
	SecondOrder= #orders{direction=2,floor=2,elevatorPID = 2},
	ThirdOrder= #orders{direction=1,floor=2,elevatorPID = 2},


	NextOrder = #orders{direction=0,floor =0, elevatorPID =0},

	RecordList = [FirstOrder,SecondOrder,ThirdOrder],

	%io:format("hei~n").
 	%SortedOrders = get_sorted_orders(),
	%CurrentFloor = get_floor(FSM_PID),

	try lists:foreach(fun(N) -> 
		case FSM_PID == N#orders.elevatorPID of
			true ->
				%NextOrder = N,
				io:format("Order found~n"),
				if 
					CurrentFloor < N#orders.floor ->
						io:format("up~n"),
						up;
					CurrentFloor > N#orders.floor ->
						down;
					CurrentFloor == N#orders.floor ->
						open
				end,
				throw(order_found);
			false ->

				ok
		end
		end, RecordList)


	catch
		throw:order_found ->
			if 
				CurrentFloor < NextOrder#orders.floor ->
					%up;
					ok;
				CurrentFloor > NextOrder#orders.floor ->
					%down;
					ok;
				CurrentFloor == NextOrder#orders.floor ->
					%open
					ok
			end
	end.
