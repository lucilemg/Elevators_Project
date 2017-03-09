- Why is the -lei and -lerl_interface included in the makefile? Having it there causes an error, while things seem to work without them.
- Driver works, FSM cant communicate with listener/scheduler for some weird reason

- Single elevator can follow command (and command only) orders in chronological order
- htop is handy, elev_port does not always exit properly
- Handling several changes to the lists in one function call is a challenge, both in orderlist_handler and statuslist_handler
- Cost function needs some actual logic
- Main priority next time: solve challenges mentioned above


- Cost function is functional and tested on single elevator, seems to work decently
- Button lights are now managed by a button_light_manager, works as intended

- Network module improved:
  - ConnectionList is added to records.hrl along with NameList
