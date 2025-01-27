#include <hammer/hammer.h>

HParser *create_modbus_parser(void) {
    // Modbus RTU frame structure
    HParser *address = h_uint8(); // Address: 1 byte
    HParser *function_code = h_uint8(); // Function Code: 1 byte

    // Data: Variable length, typically based on function code.
    // Here assuming data might be up to 252 bytes max for simplicity
    HParser *data_length = h_uint8(); // Length of data (optional, fixes data length parsing)
    HParser *data = h_repeat(h_uint8(), 0, 252);

    // Error check field, CRC16: 2 bytes
    HParser *crc = h_uint16();

    // Modbus RTU frame parser: address + function code + data + crc
    HParser *modbus_rtu_frame = h_sequence(address, function_code, data, crc, NULL);

    // Adding constraints based on common modbus RTU frame rules
    modbus_rtu_frame = h_with_constraint(modbus_rtu_frame, 
        h_fill( 
            h_bind(h_offset(2), data),
            h_act(data_length, 
                h_fn_t(0, 0, 2, 0, NULL,
                    h_uint8(), 
                    h_ignore(h_puint8())))));

    return modbus_rtu_frame;
}

int main(int argc, char **argv) {
    // Create the parser
    HParser *parser = create_modbus_parser();

    // Input data (example)
    unsigned char input_data[] = {0x01, 0x03, 0x02, 0xAB, 0xCD, 0x79, 0x84};

    // Parse the input data
    HParseResult *result = h_parse(parser, input_data, sizeof(input_data));
    if (result) {
        printf("Parsing succeeded.\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    // Free parser
    h_parser_free(parser);

    return 0;
}