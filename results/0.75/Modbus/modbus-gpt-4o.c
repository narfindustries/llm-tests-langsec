#include <hammer/hammer.h>

HParser *modbus_parser(void) {
    // Define basic types
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    
    // Modbus header
    HParser *transaction_id = uint16;
    HParser *protocol_id = uint16;
    HParser *length = uint16;
    HParser *unit_id = uint8;
    
    // Modbus PDU
    HParser *function_code = uint8;
    HParser *data = h_repeat(h_uint8(), h_data_size_field(length, sizeof(uint16_t)));
    
    // Complete Modbus packet
    HParser *modbus_packet = h_sequence(
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data,
        NULL
    );
    
    return modbus_packet;
}

int main(int argc, char **argv) {
    HParser *parser = modbus_parser();
    
    // Example of usage with binary data
    const uint8_t example_data[] = {
        0x00, 0x01, // Transaction ID
        0x00, 0x00, // Protocol ID
        0x00, 0x06, // Length
        0x11,       // Unit ID
        0x03,       // Function code
        0x00, 0x01, 0x00, 0x01  // Data
    };
    size_t len = sizeof(example_data);

    HParseResult *result = h_parse(parser, example_data, len);
    if (result) {
        printf("Parsing succeeded.\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    h_parser_free(parser);
    
    return 0;
}