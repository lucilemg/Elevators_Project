
#include "elevator_io_device.h"

#include <assert.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>

#include "con_load.h"
#include "driver/channels.h"
#include "driver/io.h"


static int elev_read_floorSensor(void);
static int elev_read_requestButton(int floor, Button button);
static int elev_read_stopButton(void);
static int elev_read_obstruction(void);

static void elev_write_floorIndicator(int floor);
static void elev_write_requestButtonLight(int floor, Button button, int value);
static void elev_write_doorLight(int value);
static void elev_write_stopButtonLight(int value);
static void elev_write_motorDirection(Dirn dirn);

typedef enum {
    ET_Comedi,
    ET_Simulation
} ElevatorType;

static ElevatorType et = ET_Simulation;
static int sockfd;

static void __attribute__((constructor)) elev_init(void){

    con_load("elevator.con",
        con_enum("elevatorType", &et,
            con_match(ET_Simulation)
            con_match(ET_Comedi)
        )
    )
    
    switch(et) {
    case ET_Comedi:
        ;
        int success = io_init();
        assert(success && "Elevator hardware initialization failed");

        for(int floor = 0; floor < N_FLOORS; floor++) {
            for(Button btn = 0; btn < N_BUTTONS; btn++){
                elev_write_requestButtonLight(floor, btn, 0);
            }
        }

        elev_write_stopButtonLight(0);
        elev_write_doorLight(0);
        elev_write_floorIndicator(0);
        break;

    case ET_Simulation:
        ;
        char ip[16] = {0};
        char port[8] = {0};
        con_load("simulator.con",
            con_val("com_ip",   ip,   "%s")
            con_val("com_port", port, "%s")
        )
    
        sockfd = socket(AF_INET, SOCK_STREAM, 0);
        assert(sockfd != -1 && "Unable to set up socket");

        struct addrinfo hints = {
            .ai_family      = AF_UNSPEC, 
            .ai_socktype    = SOCK_STREAM, 
            .ai_protocol    = IPPROTO_TCP,
        };
        struct addrinfo* res;
        getaddrinfo(ip, port, &hints, &res);

        int fail = connect(sockfd, res->ai_addr, res->ai_addrlen);
        assert(fail == 0 && "Unable to connect to simulator backend");

        freeaddrinfo(res);

        send(sockfd, (char[4]){0}, 4, 0);

        break;
    }
}


ElevInputDevice elevio_getInputDevice(void){
    return (ElevInputDevice){
        .floorSensor    = &elev_read_floorSensor,
        .requestButton  = &elev_read_requestButton,
        .stopButton     = &elev_read_stopButton,
        .obstruction    = &elev_read_obstruction
    };
}


ElevOutputDevice elevio_getOutputDevice(void){
    return (ElevOutputDevice){
        .floorIndicator     = &elev_write_floorIndicator,
        .requestButtonLight = &elev_write_requestButtonLight,
        .doorLight          = &elev_write_doorLight,
        .stopButtonLight    = &elev_write_stopButtonLight,
        .motorDirection     = &elev_write_motorDirection
    };
}


char* elevio_dirn_toString(Dirn d){
    return
        d == D_Up    ? "D_Up"         :
        d == D_Down  ? "D_Down"       :
        d == D_Stop  ? "D_Stop"       :
                       "D_UNDEFINED"  ;
}


char* elevio_button_toString(Button b){
    return
        b == B_HallUp       ? "B_HallUp"        :
        b == B_HallDown     ? "B_HallDown"      :
        b == B_Cab          ? "B_Cab"           :
                              "B_UNDEFINED"     ;
}





static const int floorSensorChannels[N_FLOORS] = {
    SENSOR_FLOOR1,
    SENSOR_FLOOR2,
    SENSOR_FLOOR3,
    SENSOR_FLOOR4,
};

static int elev_read_floorSensor(void){
    switch(et) {
    case ET_Comedi:
        for(int f = 0; f < N_FLOORS; f++){
            if(io_read_bit(floorSensorChannels[f])){
                return f;
            }
        }
        return -1;
    case ET_Simulation:
        send(sockfd, (char[4]){7}, 4, 0);
        unsigned char buf[4];
        recv(sockfd, buf, 4, 0);
        return buf[1] ? buf[2] : -1;
    }
    return -2;
}


static const int buttonChannels[N_FLOORS][N_BUTTONS] = {
    {BUTTON_UP1, BUTTON_DOWN1, BUTTON_COMMAND1},
    {BUTTON_UP2, BUTTON_DOWN2, BUTTON_COMMAND2},
    {BUTTON_UP3, BUTTON_DOWN3, BUTTON_COMMAND3},
    {BUTTON_UP4, BUTTON_DOWN4, BUTTON_COMMAND4},
};

static int elev_read_requestButton(int floor, Button button){
    switch(et) {
    case ET_Comedi:
        assert(floor >= 0);
        assert(floor < N_FLOORS);
        assert(button >= 0);
        assert(button < N_BUTTONS);

        return io_read_bit(buttonChannels[floor][button]);
    case ET_Simulation:
        send(sockfd, (char[4]){6, button, floor}, 4, 0);
        char buf[4];
        recv(sockfd, buf, 4, 0);
        return buf[1];
    }
    return 0;
}


static int elev_read_stopButton(void){
    switch(et) {
    case ET_Comedi:
        return io_read_bit(STOP);
    case ET_Simulation:
        send(sockfd, (char[4]){8}, 4, 0);
        char buf[4];
        recv(sockfd, buf, 4, 0);
        return buf[1];
    }
    return 0;
}


static int elev_read_obstruction(void){
    switch(et) {
    case ET_Comedi:
        return io_read_bit(OBSTRUCTION);
    case ET_Simulation:
        send(sockfd, (char[4]){9}, 4, 0);
        char buf[4];
        recv(sockfd, buf, 4, 0);
        return buf[1];
    }
    return 0;
}





static void elev_write_floorIndicator(int floor){
    switch(et) {
    case ET_Comedi:
        assert(floor >= 0);
        assert(floor < N_FLOORS);

        if(floor & 0x02){
            io_set_bit(LIGHT_FLOOR_IND1);
        } else {
            io_clear_bit(LIGHT_FLOOR_IND1);
        }

        if(floor & 0x01){
            io_set_bit(LIGHT_FLOOR_IND2);
        } else {
            io_clear_bit(LIGHT_FLOOR_IND2);
        }
        break;
    case ET_Simulation:
        send(sockfd, (char[4]){3, floor}, 4, 0);
        break;
    }
}


static const int buttonLightChannels[N_FLOORS][N_BUTTONS] = {
    {LIGHT_UP1, LIGHT_DOWN1, LIGHT_COMMAND1},
    {LIGHT_UP2, LIGHT_DOWN2, LIGHT_COMMAND2},
    {LIGHT_UP3, LIGHT_DOWN3, LIGHT_COMMAND3},
    {LIGHT_UP4, LIGHT_DOWN4, LIGHT_COMMAND4},
};

static void elev_write_requestButtonLight(int floor, Button button, int value){
    switch(et) {
    case ET_Comedi:
        assert(floor >= 0);
        assert(floor < N_FLOORS);
        assert(button >= 0);
        assert(button < N_BUTTONS);

        if(value){
            io_set_bit(buttonLightChannels[floor][button]);
        } else {
            io_clear_bit(buttonLightChannels[floor][button]);
        }

        break;
    case ET_Simulation:
        send(sockfd, (char[4]){2, button, floor, value}, 4, 0);
        break;
    }
}


static void elev_write_doorLight(int value){
    switch(et) {
    case ET_Comedi:
        if(value){
            io_set_bit(LIGHT_DOOR_OPEN);
        } else {
            io_clear_bit(LIGHT_DOOR_OPEN);
        }
        break;
    case ET_Simulation:
        send(sockfd, (char[4]){4, value}, 4, 0);
        break;
    }
}


static void elev_write_stopButtonLight(int value){
    switch(et) {
    case ET_Comedi:
        if(value){
            io_set_bit(LIGHT_STOP);
        } else {
            io_clear_bit(LIGHT_STOP);
        }
        break;
    case ET_Simulation:
        send(sockfd, (char[4]){5, value}, 4, 0);
        break;
    }
}


static void elev_write_motorDirection(Dirn dirn){
    switch(et) {
    case ET_Comedi:
        switch(dirn){
        case D_Up:
            io_clear_bit(MOTORDIR);
            io_write_analog(MOTOR, 2800);
            break;
        case D_Down:
            io_set_bit(MOTORDIR);
            io_write_analog(MOTOR, 2800);
            break;
        case D_Stop:
        default:
            io_write_analog(MOTOR, 0);
            break;
        }
        break;
    case ET_Simulation:
        send(sockfd, (char[4]){1, dirn}, 4, 0);
        break;
    }
}





