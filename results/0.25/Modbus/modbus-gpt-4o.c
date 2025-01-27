#include <hammer/hammer.h>

typedef struct {
    uint16_t transaction_id;
    uint16_t protocol_id;
    uint16_t length;
    uint8_t unit_id;
    uint8_t function_code;
    uint8_t data[];
} modbus_packet_t;

static HParser *modbus_packet_parser;

static HParser *create_modbus_packet_parser() {
    HParser *transaction_id = h_uint16();
    HParser *protocol_id = h_uint16();
    HParser *length = h_uint16();
    HParser *unit_id = h_uint8();
    HParser *function_code = h_uint8();
    HParser *data = h_repeat(h_uint8(), h_left(length, h_uint16_val(2)));

    return h_sequence(transaction_id, protocol_id, length, unit_id, function_code, data, NULL);
}

int main(int argc, char **argv) {
    modbus_packet_parser = create_modbus_packet_parser();

    // Example usage
    const uint8_t example_data[] = {0x00, 0x01, 0x00, 0x00, 0x00, 0x06, 0x11, 0x03, 0x00, 0x6B, 0x00, 0x03};
    HParseResult *result = h_parse(modbus_packet_parser, example_data, sizeof(example_data));

    if (result) {
        modbus_packet_t *packet = (modbus_packet_t *)result->ast;
        printf("Transaction ID: %u\n", packet->transaction_id);
        printf("Protocol ID: %u\n", packet->protocol_id);
        printf("Length: %u\n", packet->length);
        printf("Unit ID: %u\n", packet->unit_id);
        printf("Function Code: %u\n", packet->function_code);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse Modbus packet\n");
    }

    h_parser_free(modbus_packet_parser);
    return 0;
}