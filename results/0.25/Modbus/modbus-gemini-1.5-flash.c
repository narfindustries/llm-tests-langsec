#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Define Modbus function codes
#define MODBUS_FC_READ_COILS 0x01
#define MODBUS_FC_READ_DISCRETE_INPUTS 0x02
#define MODBUS_FC_READ_HOLDING_REGISTERS 0x03
#define MODBUS_FC_READ_INPUT_REGISTERS 0x04
#define MODBUS_FC_WRITE_SINGLE_COIL 0x05
#define MODBUS_FC_WRITE_SINGLE_REGISTER 0x06
#define MODBUS_FC_WRITE_MULTIPLE_COILS 0x0F
#define MODBUS_FC_WRITE_MULTIPLE_REGISTERS 0x10


// Structure to represent a Modbus request
typedef struct {
    uint8_t slave_address;
    uint8_t function_code;
    uint16_t start_address;
    uint16_t quantity;
    uint8_t *data;
    uint8_t data_length;
} ModbusRequest;


// Structure to represent a Modbus response
typedef struct {
    uint8_t slave_address;
    uint8_t function_code;
    uint8_t *data;
    uint8_t data_length;
} ModbusResponse;


// Function to calculate Modbus CRC16
uint16_t modbus_crc16(uint8_t *data, uint8_t length) {
    uint16_t crc = 0xFFFF;
    for (int pos = 0; pos < length; pos++) {
        crc ^= (uint16_t)data[pos];          // XOR byte into least sig. byte of crc
        for (int i = 8; i != 0; i--) {    // Loop over each bit
            if ((crc & 0x0001) != 0) {      // If the LSB is set
                crc >>= 1;                    // Shift right and XOR 0xA001
                crc ^= 0xA001;
            } else                            // Else LSB is not set
                crc >>= 1;                    // Just shift right
        }
    }
    return crc;
}


// Function to handle Modbus requests (replace with your actual Modbus logic)
ModbusResponse* handle_modbus_request(ModbusRequest *request) {
    ModbusResponse *response = (ModbusResponse*)malloc(sizeof(ModbusResponse));
    response->slave_address = request->slave_address;
    response->function_code = request->function_code;

    //Example: Read Holding Registers
    if(request->function_code == MODBUS_FC_READ_HOLDING_REGISTERS){
        response->data_length = request->quantity * 2;
        response->data = (uint8_t*)malloc(response->data_length);
        //Simulate reading from holding registers. Replace with your actual register values.
        for(int i = 0; i < response->data_length; i+=2){
            *((uint16_t*)(response->data + i)) = request->start_address + i/2;
        }
    } else {
        response->data_length = 0;
        response->data = NULL;
    }

    return response;
}


int main() {
    // Example usage:
    ModbusRequest request;
    request.slave_address = 1;
    request.function_code = MODBUS_FC_READ_HOLDING_REGISTERS;
    request.start_address = 0;
    request.quantity = 5;
    request.data = NULL;
    request.data_length = 0;

    ModbusResponse *response = handle_modbus_request(&request);

    if(response != NULL){
        //Process the response
        printf("Response:\n");
        printf("Slave Address: %d\n", response->slave_address);
        printf("Function Code: %d\n", response->function_code);
        printf("Data Length: %d\n", response->data_length);
        for(int i = 0; i < response->data_length; i++){
            printf("%02X ", response->data[i]);
        }
        printf("\n");
        free(response->data);
        free(response);
    } else {
        printf("Error handling Modbus request\n");
    }

    return 0;
}
