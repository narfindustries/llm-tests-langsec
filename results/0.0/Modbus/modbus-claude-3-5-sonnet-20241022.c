#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t MODBUS_MIN_LENGTH = 4;
static const uint8_t MODBUS_MAX_LENGTH = 256;

HParser* init_modbus_parser(void) {
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
    HParser* function_code = h_choice(read_coils, read_discrete_inputs,
                                    read_holding_registers, read_input_registers,
                                    write_single_coil, write_single_register,
                                    write_multiple_coils, write_multiple_registers,
                                    NULL);

    // Address fields (16-bit)
    HParser* address = h_uint16();
    
    // Data fields
    HParser* quantity = h_uint16();
    HParser* value = h_uint16();
    HParser* byte_count = h_uint8();
    HParser* data = h_many(h_uint8());

    // CRC field (16-bit)
    HParser* crc = h_uint16();

    // Message structure based on function code
    HParser* read_request = h_sequence(address, quantity, NULL);
    HParser* write_single_request = h_sequence(address, value, NULL);
    HParser* write_multiple_request = h_sequence(address, quantity, byte_count, data, NULL);

    // Complete message structure
    HParser* modbus_message = h_sequence(function_code,
                                       h_choice(read_request,
                                              write_single_request,
                                              write_multiple_request,
                                              NULL),
                                       crc,
                                       NULL);

    return h_length_value(h_uint8(), modbus_message);
}

int main(int argc, char** argv) {
    HParser* parser = init_modbus_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    return 0;
}