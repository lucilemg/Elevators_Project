
-record (orders, {direction, floor, elevatorPID}).
-record (elevatorStatus, {direction, lastFloor, state, fsm_PID}).

-define(NUMBER_OF_FLOORS, 4).

-define(LOCAL,local).
-define(NETWORK,network).

-define(FSM_PID, fsm_pid).
-define(STATUSLIST_HANDLER_PID, status_pid).
-define(ORDERLIST_HANDLER_PID, order_pid).

-define(ConnectionList, ['129.241.187.151', '129.241.187.153', '127.0.0.1', '127.0.0.1']).
-define(ElevatorNameList, ['elev1','elev2','elev3','elev4']).