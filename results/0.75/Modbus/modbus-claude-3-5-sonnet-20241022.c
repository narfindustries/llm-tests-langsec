#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static const uint8_t modbus_header[] = {0x00, 0x01};

HParser *init_modbus_parser(void) {
    // Basic parsers
    HParser *byte = h_uint8();
    HParser *uint16 = h_uint16();
    
    // Function codes
    HParser *function_code = h_uint8();
    
    // Data length
    HParser *data_length = h_uint8();
    
    // Transaction ID
    HParser *transaction_id = h_uint16();
    
    // Protocol ID (always 0x0000)
    HParser *protocol_id = h_token(modbus_header, sizeof(modbus_header));
    
    // CRC
    HParser *crc = h_uint16();
    
    // Data payload - variable length based on data_length
    HParser *data = h_length_value(data_length, h_uint8());
    
    // Complete Modbus frame
    return h_sequence(transaction_id,
                     protocol_id,
                     data_length,
                     function_code, 
                     data,
                     crc,
                     NULL);
}

int main(int argc, char *argv[]) {
    HParser *parser = init_modbus_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    uint8_t test_data[] = {
        0x00, 0x01,             // Transaction ID
        0x00, 0x00,             // Protocol ID
        0x06,                   // Length
        0x01,                   // Function code
        0x00, 0x01, 0x00, 0x02, // Data
        0x00, 0x03              // CRC
    };
    
    HParseResult *result = h_parse(parser, test_data, sizeof(test_data));
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }
    
    h_parse_result_free(result);
    return 0;
}