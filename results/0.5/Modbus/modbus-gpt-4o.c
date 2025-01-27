#include <hammer/hammer.h>

// Define Modbus PDU
HParser *modbus_pdu_parser = h_sequence(
    h_uint8(),  // Function code
    h_length_value(
        h_uint8(),  // Byte count
        h_repeat_n(h_uint8(), h_bind_uint8(1))
    ),
    NULL
);

// Define Modbus ADU for TCP
HParser *modbus_adu_tcp_parser = h_sequence(
    h_uint16_be(),  // Transaction Identifier
    h_uint16_be(),  // Protocol Identifier
    h_uint16_be(),  // Length
    h_uint8(),      // Unit Identifier
    modbus_pdu_parser,
    NULL
);

// Define Modbus ADU for RTU
HParser *modbus_adu_rtu_parser = h_sequence(
    h_uint8(),      // Address
    modbus_pdu_parser,
    h_uint16_le(),  // CRC
    NULL
);

// Error handling
void handle_parse_error(HParseResult *result) {
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        exit(EXIT_FAILURE);
    }
}

int main(int argc, char **argv) {
    // Example usage for TCP
    uint8_t sample_tcp_adu[] = {
        0x00, 0x01, 0x00, 0x00, 0x00, 0x06, 0x11, 0x03, 0x00, 0x6B, 0x00, 0x03
    };

    HParseResult *result_tcp = h_parse(modbus_adu_tcp_parser, sample_tcp_adu, sizeof(sample_tcp_adu));
    handle_parse_error(result_tcp);
    h_parse_result_free(result_tcp);

    // Example usage for RTU
    uint8_t sample_rtu_adu[] = {
        0x11, 0x03, 0x00, 0x6B, 0x00, 0x03, 0x76, 0x87
    };

    HParseResult *result_rtu = h_parse(modbus_adu_rtu_parser, sample_rtu_adu, sizeof(sample_rtu_adu));
    handle_parse_error(result_rtu);
    h_parse_result_free(result_rtu);

    // Free parsers
    h_parser_free(modbus_pdu_parser);
    h_parser_free(modbus_adu_tcp_parser);
    h_parser_free(modbus_adu_rtu_parser);

    return 0;
}