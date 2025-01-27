#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a Modbus message
typedef struct {
    uint8_t transaction_id[2];
    uint8_t protocol_id[2];
    uint8_t length[2];
    uint8_t unit_id;
    uint8_t function_code;
    uint8_t start_address[2];
    uint8_t quantity[2];
} modbus_message_t;

// Structure to represent a Modbus response
typedef struct {
    uint8_t transaction_id[2];
    uint8_t protocol_id[2];
    uint8_t length[2];
    uint8_t unit_id;
    uint8_t function_code;
    uint8_t byte_count;
    uint8_t data[];
} modbus_response_t;

// Function to parse a Modbus message
modbus_message_t* parse_modbus_message(uint8_t* buffer, int length) {
    modbus_message_t* message = (modbus_message_t*) malloc(sizeof(modbus_message_t));
    message->transaction_id[0] = buffer[0];
    message->transaction_id[1] = buffer[1];
    message->protocol_id[0] = buffer[2];
    message->protocol_id[1] = buffer[3];
    message->length[0] = buffer[4];
    message->length[1] = buffer[5];
    message->unit_id = buffer[6];
    message->function_code = buffer[7];
    message->start_address[0] = buffer[8];
    message->start_address[1] = buffer[9];
    message->quantity[0] = buffer[10];
    message->quantity[1] = buffer[11];
    return message;
}

// Function to generate a Modbus response
modbus_response_t* generate_modbus_response(modbus_message_t* message) {
    modbus_response_t* response = (modbus_response_t*) malloc(sizeof(modbus_response_t) + 10);
    response->transaction_id[0] = message->transaction_id[0];
    response->transaction_id[1] = message->transaction_id[1];
    response->protocol_id[0] = message->protocol_id[0];
    response->protocol_id[1] = message->protocol_id[1];
    response->length[0] = 0x03;
    response->length[1] = 0x00;
    response->unit_id = message->unit_id;
    response->function_code = message->function_code;
    response->byte_count = 0x02;
    response->data[0] = 0x01;
    response->data[1] = 0x02;
    return response;
}

int main() {
    uint8_t buffer[] = {0x00, 0x01, 0x00, 0x00, 0x00, 0x06, 0x01, 0x03, 0x00, 0x00, 0x00, 0x06};
    modbus_message_t* message = parse_modbus_message(buffer, sizeof(buffer));
    modbus_response_t* response = generate_modbus_response(message);
    printf("Transaction ID: %02x %02x\n", response->transaction_id[0], response->transaction_id[1]);
    printf("Protocol ID: %02x %02x\n", response->protocol_id[0], response->protocol_id[1]);
    printf("Length: %02x %02x\n", response->length[0], response->length[1]);
    printf("Unit ID: %02x\n", response->unit_id);
    printf("Function Code: %02x\n", response->function_code);
    printf("Byte Count: %02x\n", response->byte_count);
    printf("Data: %02x %02x\n", response->data[0], response->data[1]);
    free(message);
    free(response);
    return 0;
}