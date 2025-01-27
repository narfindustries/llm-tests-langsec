#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Modbus Function Code Definitions
typedef enum {
    READ_COILS = 0x01,
    READ_DISCRETE_INPUTS = 0x02,
    READ_HOLDING_REGISTERS = 0x03,
    READ_INPUT_REGISTERS = 0x04,
    WRITE_SINGLE_COIL = 0x05,
    WRITE_SINGLE_REGISTER = 0x06,
    WRITE_MULTIPLE_COILS = 0x0F,
    WRITE_MULTIPLE_REGISTERS = 0x10
} ModbusFunctionCode;

// Modbus Frame Parser
static HParser* modbus_frame_parser() {
    // Slave Address (1 byte)
    HParsedToken* slave_address = h_token_uint8();

    // Function Code (1 byte)
    HParsedToken* function_code = h_token_uint8();

    // Data Payload (variable length)
    HParsedToken* data_payload = h_many(h_token_uint8());

    // CRC (2 bytes)
    HParsedToken* crc = h_sequence2(h_token_uint8(), h_token_uint8());

    // Combine all components
    return h_sequence4(slave_address, function_code, data_payload, crc);
}

// Modbus Request Validator
static bool validate_modbus_request(HParsedToken* parsed_frame) {
    if (!parsed_frame || parsed_frame->type != TT_SEQUENCE) {
        return false;
    }

    // Validate frame components
    uint8_t slave_address = parsed_frame->seq->elements[0]->uint;
    uint8_t function_code = parsed_frame->seq->elements[1]->uint;
    
    // Basic validation checks
    if (slave_address < 1 || slave_address > 247) {
        return false;
    }

    switch (function_code) {
        case READ_COILS:
        case READ_DISCRETE_INPUTS:
        case READ_HOLDING_REGISTERS:
        case READ_INPUT_REGISTERS:
        case WRITE_SINGLE_COIL:
        case WRITE_SINGLE_REGISTER:
        case WRITE_MULTIPLE_COILS:
        case WRITE_MULTIPLE_REGISTERS:
            return true;
        default:
            return false;
    }
}

// CRC16 Calculation Function
static uint16_t calculate_modbus_crc16(const uint8_t* data, size_t length) {
    uint16_t crc = 0xFFFF;
    
    for (size_t i = 0; i < length; i++) {
        crc ^= data[i];
        for (int j = 0; j < 8; j++) {
            if (crc & 0x0001) {
                crc = (crc >> 1) ^ 0xA001;
            } else {
                crc >>= 1;
            }
        }
    }
    
    return crc;
}

// Main Modbus Frame Parsing Function
static HParsedToken* parse_modbus_frame(HParseResult* result) {
    if (!result || !result->ast) {
        return NULL;
    }

    HParsedToken* parsed_frame = result->ast;
    
    if (!validate_modbus_request(parsed_frame)) {
        return NULL;
    }

    return parsed_frame;
}

int main() {
    // Initialize Hammer parser
    HParser* modbus_parser = modbus_frame_parser();
    
    // Example Modbus frame for testing
    uint8_t test_frame[] = {0x01, 0x03, 0x00, 0x00, 0x00, 0x0A, 0xC5, 0xCD};
    
    // Parse the frame
    HParseResult* result = h_parse(modbus_parser, test_frame, sizeof(test_frame));
    
    if (result && result->ast) {
        HParsedToken* parsed_frame = parse_modbus_frame(result);
        
        if (parsed_frame) {
            printf("Valid Modbus Frame Parsed Successfully\n");
        } else {
            printf("Invalid Modbus Frame\n");
        }
    } else {
        printf("Parsing Failed\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_destroy_parser(modbus_parser);

    return 0;
}