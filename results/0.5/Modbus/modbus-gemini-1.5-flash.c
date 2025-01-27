#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a Modbus register
typedef struct {
    unsigned short address;
    unsigned short value;
} ModbusRegister;

// Function to simulate reading a Modbus register
ModbusRegister readModbusRegister(unsigned short address) {
    ModbusRegister reg;
    reg.address = address;
    // Simulate reading from a Modbus device.  Replace this with actual Modbus communication.
    switch (address) {
        case 1: reg.value = 100; break;
        case 2: reg.value = 200; break;
        case 3: reg.value = 300; break;
        default: reg.value = 0; break;
    }
    return reg;
}


// Function to simulate writing to a Modbus register
int writeModbusRegister(unsigned short address, unsigned short value) {
    // Simulate writing to a Modbus device. Replace this with actual Modbus communication.
    printf("Writing value %hu to Modbus register %hu\n", value, address);
    return 0; // 0 indicates success
}


int main() {
    // Example usage
    ModbusRegister reg1 = readModbusRegister(1);
    printf("Register 1: Address=%hu, Value=%hu\n", reg1.address, reg1.value);

    ModbusRegister reg2 = readModbusRegister(2);
    printf("Register 2: Address=%hu, Value=%hu\n", reg2.address, reg2.value);

    int result = writeModbusRegister(3, 400);
    if (result == 0) {
        printf("Write successful\n");
    } else {
        printf("Write failed\n");
    }

    return 0;
}
