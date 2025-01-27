#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdint.h>

// Modbus function codes
enum {
    READ_COILS = 0x01,
    READ_DISCRETE_INPUTS = 0x02,
    READ_HOLDING_REGISTERS = 0x03,
    READ_INPUT_REGISTERS = 0x04,
    WRITE_SINGLE_COIL = 0x05,
    WRITE_SINGLE_REGISTER = 0x06,
    WRITE_MULTIPLE_COILS = 0x0F,
    WRITE_MULTIPLE_REGISTERS = 0x10
};

// Forward declaration for recursive structures
static const HParser* modbus_pdu;

// Helper parsers
static const HParser* uint8 = h_uint8();
static const HParser* uint16 = h_uint16_be();

// Address and Quantity common structure
static const HParser* addr_and_quantity() {
    return h_sequence(uint16, uint16, NULL);
}

// Multi-register/coil data response
static const HParser* data_response(uint16_t min, uint16_t max) {
    return h_sequence(uint8, h_bytes(h_int_range(h_uint16(), min, max)), NULL);
}

// Specific PDU parsers based on function codes
static const HParser* read_coils_response() {
    return data_response(1, 250); // example range
}

static const HParser* read_discrete_inputs_response() {
    return data_response(1, 250); // example range
}

static const HParser* read_holding_registers_response() {
    return data_response(1, 125); // example range
}

static const HParser* read_input_registers_response() {
    return data_response(1, 125); // example range
}

static const HParser* write_single_coil_response() {
    return addr_and_quantity();
}

static const HParser* write_single_register_response() {
    return addr_and_quantity();
}

static const HParser* write_multiple_coils_response() {
    return addr_and_quantity();
}

static const HParser* write_multiple_registers_response() {
    return addr_and_quantity();
}

// Modbus PDU parser based on function code
static const HParser* modbus_pdu() {
    return h_choice(
        h_sequence(h_ch(READ_COILS), read_coils_response(), NULL),
        h_sequence(h_ch(READ_DISCRETE_INPUTS), read_discrete_inputs_response(), NULL),
        h_sequence(h_ch(READ_HOLDING_REGISTERS), read_holding_registers_response(), NULL),
        h_sequence(h_ch(READ_INPUT_REGISTERS), read_input_registers_response(), NULL),
        h_sequence(h_ch(WRITE_SINGLE_COIL), write_single_coil_response(), NULL),
        h_sequence(h_ch(WRITE_SINGLE_REGISTER), write_single_register_response(), NULL),
        h_sequence(h_ch(WRITE_MULTIPLE_COILS), write_multiple_coils_response(), NULL),
        h_sequence(h_ch(WRITE_MULTIPLE_REGISTERS), write_multiple_registers_response(), NULL),
        NULL
    );
}

// Modbus ADU (Application Data Unit)
static const HParser* modbus_adu() {
    return h_sequence(uint8, modbus_pdu(), h_uint16_be(), NULL);
}

int main(int argc, char *argv[]) {
    // Initializing the parser
    HParser *parser = modbus_adu();

    // Example Modbus frame (as byte array for demonstration)
    uint8_t modbus_frame[] = {0x01, 0x03, 0x00, 0x6B, 0x00, 0x03, 0x76, 0x87};

    // Parse the frame
    HParseResult *result = h_parse(parser, modbus_frame, sizeof(modbus_frame));
    if (result) {
        printf("Parsing successful.\n");
        h_pprint(stdout, result->ast, 0, 1);
    } else {
        printf("Parsing failed.\n");
    }

    // Cleaning up
    h_parse_result_free(result);
    return 0;
}