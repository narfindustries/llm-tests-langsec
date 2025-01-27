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

// Define the Hammer parser for Modbus PDU
HParser *modbus_pdu_parser() {
    return h_sequence(
        h_uint8(),  // Function Code
        h_uint16(), // Starting Address
        h_uint16(), // Quantity
        NULL
    );
}

// Function to parse the Modbus PDU
ModbusPDU parse_modbus_pdu(const uint8_t *input, size_t input_len) {
    ModbusPDU pdu = {0};
    HParseResult *result = h_parse(modbus_pdu_parser(), input, input_len);
    
    if (result) {
        pdu.function_code = result->ast->seq->elements[0]->uint8;
        pdu.starting_address = result->ast->seq->elements[1]->uint16;
        pdu.quantity = result->ast->seq->elements[2]->uint16;
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse Modbus PDU\n");
        exit(1);
    }
    
    return pdu;
}

// Main function to demonstrate parsing
int main() {
    uint8_t modbus_data[] = {0x03, 0x00, 0x6B, 0x00, 0x03}; // Example Modbus PDU
    size_t modbus_data_len = sizeof(modbus_data);
    
    ModbusPDU pdu = parse_modbus_pdu(modbus_data, modbus_data_len);
    
    printf("Function Code: 0x%02X\n", pdu.function_code);
    printf("Starting Address: 0x%04X\n", pdu.starting_address);
    printf("Quantity: 0x%04X\n", pdu.quantity);
    
    return 0;
}