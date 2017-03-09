
-record (orders, {direction, floor, elevatorPID}).
-record (elevatorStatus, {direction, lastFloor, state, fsm_PID}).

-define(NUMBER_OF_FLOORS, 4).



-define(FSM_PID, fsm_pid).
-define(STATUSLIST_HANDLER_PID, status_pid).
-define(ORDERLIST_HANDLER_PID, order_pid).

-define(ConnectionList, ['elev1@127.0.0.1','elev2@127.0.0.1','elev3@127.0.0.1','elev4@127.0.0.1']).
-define(ElevatorNameList, ['elev1','elev2','elev3','elev4']).
