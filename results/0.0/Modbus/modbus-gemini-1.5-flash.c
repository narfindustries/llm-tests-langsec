#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Define Modbus function codes
#define MODBUS_FC_READ_COILS 0x01
#define MODBUS_FC_READ_DISCRETE_INPUTS 0x02
#define MODBUS_FC_READ_HOLDING_REGISTERS 0x03
#define MODBUS_FC_READ_INPUT_REGISTERS 0x04
#define MODBUS_FC_WRITE_SINGLE_COIL 0x05
#define MODBUS_FC_WRITE_SINGLE_REGISTER 0x06
#define MODBUS_FC_WRITE_MULTIPLE_COILS 0x0F
#define MODBUS_FC_WRITE_MULTIPLE_REGISTERS 0x10


// Modbus function to read holding registers
uint8_t* modbus_read_holding_registers(uint8_t slave_address, uint16_t start_address, uint16_t num_registers) {
    // Simulate reading from holding registers (replace with actual hardware interaction)
    uint8_t* data = (uint8_t*)malloc(num_registers * 2); // Each register is 2 bytes
    if (data == NULL) {
        return NULL;
    }
    for (int i = 0; i < num_registers * 2; i++) {
        data[i] = (uint8_t)(start_address + i); //Example data
    }
    return data;
}


int main() {
    // Example usage: Read 5 holding registers starting from address 100 from slave 1
    uint8_t slave_address = 1;
    uint16_t start_address = 100;
    uint16_t num_registers = 5;

    uint8_t* data = modbus_read_holding_registers(slave_address, start_address, num_registers);

    if (data != NULL) {
        printf("Read data:\n");
        for (int i = 0; i < num_registers * 2; i++) {
            printf("%02X ", data[i]);
        }
        printf("\n");
        free(data);
    } else {
        printf("Error reading data.\n");
    }

    return 0;
}
