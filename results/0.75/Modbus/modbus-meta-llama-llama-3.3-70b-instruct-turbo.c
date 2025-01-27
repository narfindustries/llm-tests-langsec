#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// Define Modbus protocol structure
typedef struct {
    uint8_t transaction_id[2];
    uint8_t protocol_id[2];
    uint8_t length[2];
    uint8_t unit_id;
    uint8_t function_code;
    uint8_t starting_address[2];
    uint8_t quantity[2];
} modbus_request_t;

typedef struct {
    uint8_t transaction_id[2];
    uint8_t protocol_id[2];
    uint8_t length[2];
    uint8_t unit_id;
    uint8_t function_code;
    uint8_t byte_count;
    uint8_t data[];
} modbus_response_t;

// Define Modbus CRC calculation function
uint16_t modbus_crc(uint8_t* data, uint16_t length) {
    uint16_t crc = 0xFFFF;
    for (uint16_t i = 0; i < length; i++) {
        crc ^= data[i];
        for (uint8_t j = 0; j < 8; j++) {
            if (crc & 1) {
                crc = (crc >> 1) ^ 0xA001;
            } else {
                crc >>= 1;
            }
        }
    }
    return crc;
}

int main() {
    // Create a sample Modbus request
    modbus_request_t request;
    request.transaction_id[0] = 0x00;
    request.transaction_id[1] = 0x01;
    request.protocol_id[0] = 0x00;
    request.protocol_id[1] = 0x00;
    request.length[0] = 0x00;
    request.length[1] = 0x06;
    request.unit_id = 0x01;
    request.function_code = 0x03;
    request.starting_address[0] = 0x00;
    request.starting_address[1] = 0x00;
    request.quantity[0] = 0x00;
    request.quantity[1] = 0x05;

    // Calculate CRC for the request
    uint16_t crc = modbus_crc((uint8_t*)&request, 6);

    // Print the request with CRC
    printf("Request: ");
    for (uint8_t i = 0; i < 6; i++) {
        printf("%02x ", ((uint8_t*)&request)[i]);
    }
    printf("%02x %02x\n", crc & 0xFF, crc >> 8);

    // Create a sample Modbus response
    modbus_response_t response;
    response.transaction_id[0] = 0x00;
    response.transaction_id[1] = 0x01;
    response.protocol_id[0] = 0x00;
    response.protocol_id[1] = 0x00;
    response.length[0] = 0x00;
    response.length[1] = 0x05;
    response.unit_id = 0x01;
    response.function_code = 0x03;
    response.byte_count = 0x02;
    response.data[0] = 0xCD;
    response.data[1] = 0x6B;

    // Calculate CRC for the response
    crc = modbus_crc((uint8_t*)&response, 7);

    // Print the response with CRC
    printf("Response: ");
    for (uint8_t i = 0; i < 7; i++) {
        printf("%02x ", ((uint8_t*)&response)[i]);
    }
    printf("%02x %02x\n", crc & 0xFF, crc >> 8);

    return 0;
}