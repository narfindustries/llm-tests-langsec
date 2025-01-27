#include <hammer/hammer.h>
#include <hammer/parsers.h>
#include <stdio.h>
#include <stdlib.h>

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
    HParsedToken* slave_address = h_token(h_ch_range(0x01, 0xF7), "SlaveAddress");

    // Function Code (1 byte)
    HParsedToken* function_code = h_token(h_ch_range(0x01, 0x10), "FunctionCode");

    // Data Length (variable)
    HParsedToken* data_length = h_token(h_ch_range(0x00, 0xFF), "DataLength");

    // Data Payload (variable)
    HParsedToken* data_payload = h_repeat_n(h_ch_range(0x00, 0xFF), 1, 252, "DataPayload");

    // CRC (2 bytes)
    HParsedToken* crc = h_repeat_n(h_ch_range(0x00, 0xFF), 2, 2, "CRC");

    // Combine parsers into complete Modbus frame
    HParser* modbus_frame = h_sequence(
        slave_address,
        function_code,
        data_length,
        data_payload,
        crc,
        NULL
    );

    return modbus_frame;
}

// Main parsing function
int main() {
    HParser* parser = modbus_frame_parser();
    
    // Example Modbus frame for testing
    const uint8_t test_frame[] = {
        0x01,       // Slave Address
        0x03,       // Function Code (Read Holding Registers)
        0x02,       // Data Length
        0x00, 0x0A, // Data Payload (Register Address)
        0xAA, 0xBB  // CRC
    };

    HParseResult* result = h_parse(parser, test_frame, sizeof(test_frame));

    if (result) {
        printf("Modbus frame parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Modbus frame parsing failed\n");
    }

    h_parser_free(parser);
    return 0;
}