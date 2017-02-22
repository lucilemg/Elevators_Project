-module(main).

-export([main/0]).


main() ->

	Namelist = ['elev1','elev2','elev3','elev4'],
	ConnectionList = ['elev1@127.0.0.1','elev2@127.0.0.1','elev3@127.0.0.1','elev4@127.0.0.1'],

	network:init_conn(ConnectionList,1),
	network:init_listener(Namelist,1),

	FSM = fsm:start(),
	elev_driver:start(FSM,elevator),
	FSM ! hardware_initialized.


