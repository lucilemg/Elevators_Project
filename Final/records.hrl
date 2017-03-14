
-define(NUMBER_OF_FLOORS, 4).


-record (orders, {direction, floor, assignedElevID, waitingTime}).
-record (elevatorStatus, {direction, lastFloor, state}).


-define(FSM_PID, fsm_pid).
-define(STATUSLIST_HANDLER_PID, status_pid).
-define(ORDERLIST_HANDLER_PID, order_pid).
-define(NETWORK_MONITOR_PID,monitor_pid).


-define(IPList, 			['129.241.187.145', '129.241.187.155', 	'127.0.0.1']).
-define(ElevatorNameList, 	['elev1',			'elev2',			'elev3']).

-define(LOCAL,local).
-define(NETWORK,network).
