#include <hammer/hammer.h>
#include <hammer/glue.h>

// Modbus function codes
#define READ_COILS 0x01
#define READ_DISCRETE_INPUTS 0x02
#define READ_HOLDING_REGISTERS 0x03
#define READ_INPUT_REGISTERS 0x04
#define WRITE_SINGLE_COIL 0x05
#define WRITE_SINGLE_REGISTER 0x06
#define WRITE_MULTIPLE_COILS 0x0F
#define WRITE_MULTIPLE_REGISTERS 0x10

// Modbus exception codes
#define ILLEGAL_FUNCTION 0x01
#define ILLEGAL_DATA_ADDRESS 0x02
#define ILLEGAL_DATA_VALUE 0x03
#define SERVER_DEVICE_FAILURE 0x04
#define ACKNOWLEDGE 0x05
#define SERVER_DEVICE_BUSY 0x06
#define MEMORY_PARITY_ERROR 0x08
#define GATEWAY_PATH_UNAVAILABLE 0x0A
#define GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND 0x0B

static HParser *modbus_address;
static HParser *modbus_value;
static HParser *modbus_quantity;
static HParser *modbus_exception_code;

static HParsedToken *act_identity(const HParseResult *p, void *user_data) {
    return h_act_identity(p, user_data);
}

static void init_parsers() {
    modbus_address = h_uint16();
    modbus_value = h_uint16();
    modbus_quantity = h_uint16();
    modbus_exception_code = h_uint8();
}

static HParser *modbus_request_pdu() {
    H_RULE(function_code, h_uint8());
    H_ARULE(read_coils, h_sequence(function_code, modbus_address, modbus_quantity, NULL));
    H_ARULE(read_discrete_inputs, h_sequence(function_code, modbus_address, modbus_quantity, NULL));
    H_ARULE(read_holding_registers, h_sequence(function_code, modbus_address, modbus_quantity, NULL));
    H_ARULE(read_input_registers, h_sequence(function_code, modbus_address, modbus_quantity, NULL));
    H_ARULE(write_single_coil, h_sequence(function_code, modbus_address, modbus_value, NULL));
    H_ARULE(write_single_register, h_sequence(function_code, modbus_address, modbus_value, NULL));
    H_ARULE(write_multiple_coils, h_sequence(function_code, modbus_address, modbus_quantity, h_many(modbus_value), NULL));
    H_ARULE(write_multiple_registers, h_sequence(function_code, modbus_address, modbus_quantity, h_many(modbus_value), NULL));

    H_RULE(modbus_request, h_choice(read_coils, read_discrete_inputs, read_holding_registers, read_input_registers,
                                    write_single_coil, write_single_register, write_multiple_coils, write_multiple_registers, NULL));

    return modbus_request;
}

static HParser *modbus_response_pdu() {
    H_RULE(function_code, h_uint8());
    H_RULE(exception, h_sequence(function_code, modbus_exception_code, NULL));
    H_RULE(data, h_bytes(1));

    H_RULE(response, h_choice(exception, h_sequence(function_code, data, NULL), NULL));

    return response;
}

static HParser *modbus_adu() {
    H_RULE(transaction_id, h_uint16());
    H_RULE(protocol_id, h_uint16());
    H_RULE(length, h_uint16());
    H_RULE(unit_id, h_uint8());

    H_RULE(request_adu, h_sequence(transaction_id, protocol_id, length, unit_id, modbus_request_pdu(), NULL));
    H_RULE(response_adu, h_sequence(transaction_id, protocol_id, length, unit_id, modbus_response_pdu(), NULL));

    return h_choice(request_adu, response_adu, NULL);
}

int main(int argc, char *argv[]) {
    HParser *modbus_parser;
    init_parsers();
    modbus_parser = modbus_adu();

    // Compile and use the parser as needed
    return 0;
}