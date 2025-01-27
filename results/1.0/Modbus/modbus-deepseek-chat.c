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
    uint8_t unit_id;
    ModbusPDU pdu;
} ModbusADU;

// Hammer parser for Modbus PDU
HParser *modbus_pdu_parser() {
    return h_sequence(
        h_uint8(),  // function_code
        h_uint16(), // starting_address
        h_uint16(), // quantity
        NULL
    );
}

// Hammer parser for Modbus ADU
HParser *modbus_adu_parser() {
    return h_sequence(
        h_uint8(),  // transaction_id
        h_uint8(),  // protocol_id
        h_uint16(), // length
        h_uint8(),  // unit_id
        modbus_pdu_parser(), // pdu
        NULL
    );
}

// Function to parse Modbus ADU
ModbusADU *parse_modbus_adu(const uint8_t *data, size_t length) {
    HParser *parser = modbus_adu_parser();
    HParseResult *result = h_parse(parser, data, length);
    if (!result) {
        return NULL;
    }

    ModbusADU *adu = (ModbusADU *)malloc(sizeof(ModbusADU));
    adu->transaction_id = ((uint8_t *)result->ast->data)[0];
    adu->protocol_id = ((uint8_t *)result->ast->data)[1];
    adu->length = ((uint16_t *)result->ast->data)[1];
    adu->unit_id = ((uint8_t *)result->ast->data)[4];
    adu->pdu.function_code = ((uint8_t *)result->ast->data)[5];
    adu->pdu.starting_address = ((uint16_t *)result->ast->data)[3];
    adu->pdu.quantity = ((uint16_t *)result->ast->data)[4];

    h_parse_result_free(result);
    return adu;
}

// Function to free Modbus ADU
void free_modbus_adu(ModbusADU *adu) {
    if (adu) {
        free(adu);
    }
}

int main() {
    // Example Modbus ADU data
    uint8_t data[] = {0x00, 0x01, 0x00, 0x00, 0x01, 0x03, 0x00, 0x01, 0x00, 0x02};
    size_t length = sizeof(data);

    ModbusADU *adu = parse_modbus_adu(data, length);
    if (adu) {
        printf("Transaction ID: %02x\n", adu->transaction_id);
        printf("Protocol ID: %02x\n", adu->protocol_id);
        printf("Length: %04x\n", adu->length);
        printf("Unit ID: %02x\n", adu->unit_id);
        printf("Function Code: %02x\n", adu->pdu.function_code);
        printf("Starting Address: %04x\n", adu->pdu.starting_address);
        printf("Quantity: %04x\n", adu->pdu.quantity);

        free_modbus_adu(adu);
    } else {
        printf("Failed to parse Modbus ADU\n");
    }

    return 0;
}