#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the Modbus PDU structure
typedef struct {
    uint8_t function_code;
    uint16_t starting_address;
    uint16_t quantity;
} ModbusPDU;

// Define the Modbus ADU structure
typedef struct {
    uint8_t transaction_id;
    uint8_t protocol_id;
    uint16_t length;
    ModbusPDU pdu;
} ModbusADU;

// Define the Hammer parser for Modbus PDU
HParser *modbus_pdu_parser() {
    return h_sequence(
        h_uint8(),  // function_code
        h_uint16(), // starting_address
        h_uint16(), // quantity
        NULL
    );
}

// Define the Hammer parser for Modbus ADU
HParser *modbus_adu_parser() {
    return h_sequence(
        h_uint8(),  // transaction_id
        h_uint8(),  // protocol_id
        h_uint16(), // length
        modbus_pdu_parser(), // pdu
        NULL
    );
}

// Function to parse Modbus ADU from a buffer
ModbusADU *parse_modbus_adu(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(modbus_adu_parser(), data, length);
    if (result == NULL) {
        fprintf(stderr, "Failed to parse Modbus ADU\n");
        return NULL;
    }

    ModbusADU *adu = (ModbusADU *)malloc(sizeof(ModbusADU));
    if (adu == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        h_parse_result_free(result);
        return NULL;
    }

    adu->transaction_id = ((uint8_t *)result->ast->data)[0];
    adu->protocol_id = ((uint8_t *)result->ast->data)[1];
    adu->length = ((uint16_t *)result->ast->data)[1];
    adu->pdu.function_code = ((uint8_t *)result->ast->data)[4];
    adu->pdu.starting_address = ((uint16_t *)result->ast->data)[2];
    adu->pdu.quantity = ((uint16_t *)result->ast->data)[3];

    h_parse_result_free(result);
    return adu;
}

// Function to free the Modbus ADU structure
void free_modbus_adu(ModbusADU *adu) {
    if (adu != NULL) {
        free(adu);
    }
}

int main() {
    // Example Modbus ADU data
    uint8_t modbus_data[] = {
        0x01, 0x00, 0x00, 0x06, 0x03, 0x00, 0x01, 0x00, 0x02
    };

    ModbusADU *adu = parse_modbus_adu(modbus_data, sizeof(modbus_data));
    if (adu != NULL) {
        printf("Transaction ID: %02x\n", adu->transaction_id);
        printf("Protocol ID: %02x\n", adu->protocol_id);
        printf("Length: %04x\n", adu->length);
        printf("Function Code: %02x\n", adu->pdu.function_code);
        printf("Starting Address: %04x\n", adu->pdu.starting_address);
        printf("Quantity: %04x\n", adu->pdu.quantity);

        free_modbus_adu(adu);
    }

    return 0;
}