#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t MODBUS_BROADCAST_ADDR = 0x00;
static const uint8_t MODBUS_MIN_ADDR = 0x01;
static const uint8_t MODBUS_MAX_ADDR = 0xFF;

static HParser* init_modbus_parser(void) {
    // Function codes
    HParser* read_coils = h_int_range(h_uint8(), 0x01, 0x01);
    HParser* read_discrete_inputs = h_int_range(h_uint8(), 0x02, 0x02);
    HParser* read_holding_registers = h_int_range(h_uint8(), 0x03, 0x03);
    HParser* read_input_registers = h_int_range(h_uint8(), 0x04, 0x04);
    HParser* write_single_coil = h_int_range(h_uint8(), 0x05, 0x05);
    HParser* write_single_register = h_int_range(h_uint8(), 0x06, 0x06);
    HParser* write_multiple_coils = h_int_range(h_uint8(), 0x0F, 0x0F);
    HParser* write_multiple_registers = h_int_range(h_uint8(), 0x10, 0x10);

    // Function code parser
    HParser* function_code = h_choice(h_sequence(read_coils, NULL),
                                    h_sequence(read_discrete_inputs, NULL),
                                    h_sequence(read_holding_registers, NULL),
                                    h_sequence(read_input_registers, NULL),
                                    h_sequence(write_single_coil, NULL),
                                    h_sequence(write_single_register, NULL),
                                    h_sequence(write_multiple_coils, NULL),
                                    h_sequence(write_multiple_registers, NULL),
                                    NULL);

    // Address parser
    HParser* address = h_int_range(h_uint8(), MODBUS_MIN_ADDR, MODBUS_MAX_ADDR);

    // Data parser
    HParser* data = h_many(h_uint8());

    // CRC parser
    HParser* crc = h_uint16();

    // Complete Modbus frame
    return h_sequence(address,
                     function_code,
                     data,
                     crc,
                     NULL);
}

int main(int argc, char** argv) {
    HParser* modbus_parser = init_modbus_parser();
    if (!modbus_parser) {
        fprintf(stderr, "Failed to initialize Modbus parser\n");
        return 1;
    }

    // Example usage
    uint8_t test_data[] = {0x01, 0x03, 0x00, 0x00, 0x00, 0x0A, 0xC5, 0xCD};
    size_t input_size = sizeof(test_data);
    
    HParseResult* result = h_parse(modbus_parser, test_data, input_size);
    if (result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    return 0;
}