#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

// Modbus ADU structure
typedef struct {
    uint8_t transaction_id[2];
    uint8_t protocol_id[2];
    uint8_t length[2];
    uint8_t unit_id;
    uint8_t function_code;
    uint8_t data[];
} modbus_adu_t;

// Modbus PDU structure
typedef struct {
    uint8_t function_code;
    uint8_t data[];
} modbus_pdu_t;

// Modbus message structure
typedef struct {
    modbus_adu_t adu;
    modbus_pdu_t pdu;
} modbus_message_t;

// Function to parse Modbus message
modbus_message_t* parse_modbus_message(uint8_t* buffer, size_t length) {
    modbus_message_t* message = (modbus_message_t*)malloc(sizeof(modbus_message_t));
    message->adu.transaction_id[0] = buffer[0];
    message->adu.transaction_id[1] = buffer[1];
    message->adu.protocol_id[0] = buffer[2];
    message->adu.protocol_id[1] = buffer[3];
    message->adu.length[0] = buffer[4];
    message->adu.length[1] = buffer[5];
    message->adu.unit_id = buffer[6];
    message->adu.function_code = buffer[7];
    message->pdu.function_code = buffer[7];
    message->pdu.data = buffer + 8;
    return message;
}

// Function to generate Modbus message
uint8_t* generate_modbus_message(modbus_message_t* message, size_t* length) {
    *length = 8 + message->pdu.data[0];
    uint8_t* buffer = (uint8_t*)malloc(*length);
    buffer[0] = message->adu.transaction_id[0];
    buffer[1] = message->adu.transaction_id[1];
    buffer[2] = message->adu.protocol_id[0];
    buffer[3] = message->adu.protocol_id[1];
    buffer[4] = (message->adu.length[0] >> 8) & 0xFF;
    buffer[5] = message->adu.length[0] & 0xFF;
    buffer[6] = message->adu.unit_id;
    buffer[7] = message->adu.function_code;
    memcpy(buffer + 8, message->pdu.data, message->pdu.data[0]);
    return buffer;
}

int main() {
    // Example usage:
    modbus_message_t message;
    message.adu.transaction_id[0] = 0x01;
    message.adu.transaction_id[1] = 0x02;
    message.adu.protocol_id[0] = 0x00;
    message.adu.protocol_id[1] = 0x00;
    message.adu.length[0] = 0x06;
    message.adu.unit_id = 0x01;
    message.adu.function_code = 0x10;
    message.pdu.function_code = 0x10;
    message.pdu.data[0] = 0x02;
    message.pdu.data[1] = 0x00;
    message.pdu.data[2] = 0x01;
    size_t length;
    uint8_t* buffer = generate_modbus_message(&message, &length);
    modbus_message_t* parsed_message = parse_modbus_message(buffer, length);
    printf("Transaction ID: %02x%02x\n", parsed_message->adu.transaction_id[0], parsed_message->adu.transaction_id[1]);
    printf("Protocol ID: %02x%02x\n", parsed_message->adu.protocol_id[0], parsed_message->adu.protocol_id[1]);
    printf("Length: %02x%02x\n", parsed_message->adu.length[0], parsed_message->adu.length[1]);
    printf("Unit ID: %02x\n", parsed_message->adu.unit_id);
    printf("Function Code: %02x\n", parsed_message->adu.function_code);
    printf("Data: ");
    for (size_t i = 0; i < length - 8; i++) {
        printf("%02x ", parsed_message->pdu.data[i]);
    }
    printf("\n");
    free(buffer);
    free(parsed_message);
    return 0;
}