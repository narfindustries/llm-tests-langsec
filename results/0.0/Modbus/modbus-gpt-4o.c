#include <hammer/hammer.h>

HParser *modbus_parser(void) {
    // Define basic types
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();

    // Define Modbus frame structure
    HParser *transaction_id = uint16;
    HParser *protocol_id = uint16;
    HParser *length = uint16;
    HParser *unit_id = uint8;
    HParser *function_code = uint8;

    // Define data field based on function code
    HParser *data = h_choice(
        h_sequence(uint8, uint8, NULL), // Example for a specific function code
        h_sequence(uint16, uint16, NULL), // Another example
        NULL
    );

    // Define Modbus frame
    HParser *modbus_frame = h_sequence(
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data,
        NULL
    );

    // Return the complete parser
    return modbus_frame;
}

int main(int argc, char **argv) {
    // Create the parser
    HParser *parser = modbus_parser();

    // Example usage: parse a Modbus frame
    const uint8_t modbus_data[] = {0x00, 0x01, 0x00, 0x00, 0x00, 0x06, 0x11, 0x03, 0x00, 0x6B, 0x00, 0x03};
    HParseResult *result = h_parse(parser, modbus_data, sizeof(modbus_data));

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    // Free the parser
    h_parser_free(parser);

    return 0;
}