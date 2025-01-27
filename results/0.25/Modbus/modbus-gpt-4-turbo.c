#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the Modbus function codes
#define READ_COILS 0x01
#define READ_DISCRETE_INPUTS 0x02
#define READ_HOLDING_REGISTERS 0x03
#define READ_INPUT_REGISTERS 0x04
#define WRITE_SINGLE_COIL 0x05
#define WRITE_SINGLE_REGISTER 0x06
#define WRITE_MULTIPLE_COILS 0x0F
#define WRITE_MULTIPLE_REGISTERS 0x10

// Define the Modbus exception codes
#define ILLEGAL_FUNCTION 0x01
#define ILLEGAL_DATA_ADDRESS 0x02
#define ILLEGAL_DATA_VALUE 0x03
#define SERVER_DEVICE_FAILURE 0x04
#define ACKNOWLEDGE 0x05
#define SERVER_DEVICE_BUSY 0x06
#define MEMORY_PARITY_ERROR 0x08
#define GATEWAY_PATH_UNAVAILABLE 0x0A
#define GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND 0x0B

// Modbus PDU (Protocol Data Unit)
static HParser *modbus_pdu;

// Modbus ADU (Application Data Unit) - RTU (Remote Terminal Unit) Frame
static HParser *modbus_rtu_adu;

// Helper parsers for data types
static HParser *uint8 = h_uint8();
static HParser *uint16 = h_uint16_le();

// Helper function to create a sequence parser for a fixed count of uint16
static HParser *uint16_seq(size_t count) {
    return h_repeat_n(uint16, count);
}

// Initialize parsers for different Modbus function codes
static void init_modbus_pdu() {
    H_RULE(read_coils, h_sequence(uint16, uint16, NULL));
    H_RULE(read_discrete_inputs, h_sequence(uint16, uint16, NULL));
    H_RULE(read_holding_registers, h_sequence(uint16, uint16, NULL));
    H_RULE(read_input_registers, h_sequence(uint16, uint16, NULL));
    H_RULE(write_single_coil, h_sequence(uint16, uint16, NULL));
    H_RULE(write_single_register, h_sequence(uint16, uint16, NULL));
    H_RULE(write_multiple_coils, h_sequence(uint16, uint16, h_length_value(uint16, uint8), NULL));
    H_RULE(write_multiple_registers, h_sequence(uint16, uint16, h_length_value(uint16, uint8), NULL));

    modbus_pdu = h_choice(
        h_sequence(h_ch(READ_COILS), read_coils),
        h_sequence(h_ch(READ_DISCRETE_INPUTS), read_discrete_inputs),
        h_sequence(h_ch(READ_HOLDING_REGISTERS), read_holding_registers),
        h_sequence(h_ch(READ_INPUT_REGISTERS), read_input_registers),
        h_sequence(h_ch(WRITE_SINGLE_COIL), write_single_coil),
        h_sequence(h_ch(WRITE_SINGLE_REGISTER), write_single_register),
        h_sequence(h_ch(WRITE_MULTIPLE_COILS), write_multiple_coils),
        h_sequence(h_ch(WRITE_MULTIPLE_REGISTERS), write_multiple_registers),
        NULL
    );
}

// Initialize the Modbus RTU ADU parser
static void init_modbus_rtu_adu() {
    H_RULE(slave_address, uint8);
    H_RULE(crc, uint16);

    modbus_rtu_adu = h_sequence(slave_address, modbus_pdu, crc, NULL);
}

int main(int argc, char **argv) {
    init_modbus_pdu();
    init_modbus_rtu_adu();

    // Example usage of the parser
    uint8_t example_input[] = {0x01, 0x03, 0x00, 0x6B, 0x00, 0x03, 0x76, 0x87};
    HParseResult *result = h_parse(modbus_rtu_adu, example_input, sizeof(example_input));

    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Parse failed.\n");
    }

    return 0;
}