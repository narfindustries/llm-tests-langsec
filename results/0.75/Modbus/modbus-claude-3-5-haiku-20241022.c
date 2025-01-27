#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdint.h>

// Modbus function codes
typedef enum {
    READ_COILS = 0x01,
    READ_DISCRETE_INPUTS = 0x02,
    READ_HOLDING_REGISTERS = 0x03,
    READ_INPUT_REGISTERS = 0x04,
    WRITE_SINGLE_COIL = 0x05,
    WRITE_SINGLE_REGISTER = 0x06
} ModbusFunctionCode;

// Modbus frame parser
static HParser* modbus_parser;

// Function to create Modbus frame parser
static HParser* create_modbus_parser() {
    // Slave address (1 byte)
    HParser* slave_address = h_uint8();

    // Function code (1 byte)
    HParser* function_code = h_uint8();

    // Data payload (variable length depending on function code)
    HParser* data_payload = h_many(h_uint8());

    // CRC (2 bytes)
    HParser* crc = h_uint16();

    // Complete Modbus frame structure
    return h_sequence(
        slave_address,
        function_code, 
        data_payload,
        crc,
        NULL
    );
}

// Validation function
static bool validate_modbus_frame(HParseResult* result) {
    if (!result || !result->ast) return false;

    HCountedArray* frame = (HCountedArray*)result->ast;
    if (frame->len < 4) return false;

    uint8_t slave_address = frame->data[0];
    uint8_t function_code = frame->data[1];

    // Basic validation checks
    if (slave_address < 1 || slave_address > 247) return false;
    
    switch (function_code) {
        case READ_COILS:
        case READ_DISCRETE_INPUTS:
        case READ_HOLDING_REGISTERS:
        case READ_INPUT_REGISTERS:
        case WRITE_SINGLE_COIL:
        case WRITE_SINGLE_REGISTER:
            return true;
        default:
            return false;
    }
}

// Main parsing function
int parse_modbus_frame(uint8_t* data, size_t len) {
    // Initialize Hammer parser
    modbus_parser = create_modbus_parser();

    // Parse the input data
    HParseResult* result = h_parse(modbus_parser, data, len);

    if (result && validate_modbus_frame(result)) {
        printf("Valid Modbus frame parsed successfully\n");
        return 0;
    } else {
        printf("Invalid Modbus frame\n");
        return -1;
    }
}

int main() {
    // Example Modbus frame for testing
    uint8_t test_frame[] = {0x01, 0x03, 0x00, 0x00, 0x00, 0x0A, 0xC5, 0xCD};
    
    int result = parse_modbus_frame(test_frame, sizeof(test_frame));
    
    return result;
}