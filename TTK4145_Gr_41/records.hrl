
-record (orders, {direction, floor, elevatorPID}).
-record (elevatorStatus, {direction, lastFloor, state, fsm_PID}).

-define(FSM_PID, fsm_pid).
-define(STATUSLIST_HANDLER_PID, status_pid).
-define(ORDERLIST_HANDLER_PID, order_pid).
