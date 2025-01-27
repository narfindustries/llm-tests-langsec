#include <hammer/hammer.h>
#include <hammer/glue.h>

// Modbus Function Codes
#define READ_COILS 0x01
#define READ_DISCRETE_INPUTS 0x02
#define READ_HOLDING_REGISTERS 0x03
#define READ_INPUT_REGISTERS 0x04
#define WRITE_SINGLE_COIL 0x05
#define WRITE_SINGLE_REGISTER 0x06
#define WRITE_MULTIPLE_COILS 0x0F
#define WRITE_MULTIPLE_REGISTERS 0x10

// Modbus Exception Codes
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
static HParser *modbus_quantity;
static HParser *modbus_value;
static HParser *modbus_exception_code;

static HParser *modbus_pdu;
static HParser *modbus_request;
static HParser *modbus_response;

static void init_modbus_parsers() {
    modbus_address = h_uint16();
    modbus_quantity = h_uint16();
    modbus_value = h_uint16();
    modbus_exception_code = h_uint8();

    HParser *modbus_read_request = h_sequence(modbus_address, modbus_quantity, NULL);
    HParser *modbus_write_single_request = h_sequence(modbus_address, modbus_value, NULL);
    HParser *modbus_write_multiple_request = h_sequence(modbus_address, modbus_quantity, h_length_value(h_uint8(), h_repeat_n(modbus_value, h_indirect())),
                                                        NULL);

    HParser *modbus_read_response = h_length_value(h_uint8(), h_repeat_n(modbus_value, h_indirect()));
    HParser *modbus_write_response = h_sequence(modbus_address, modbus_quantity, NULL);
    HParser *modbus_exception_response = h_sequence(h_uint8(), modbus_exception_code, NULL);

    modbus_request = h_choice(
        h_sequence(h_uint8_const(READ_COILS), modbus_read_request),
        h_sequence(h_uint8_const(READ_DISCRETE_INPUTS), modbus_read_request),
        h_sequence(h_uint8_const(READ_HOLDING_REGISTERS), modbus_read_request),
        h_sequence(h_uint8_const(READ_INPUT_REGISTERS), modbus_read_request),
        h_sequence(h_uint8_const(WRITE_SINGLE_COIL), modbus_write_single_request),
        h_sequence(h_uint8_const(WRITE_SINGLE_REGISTER), modbus_write_single_request),
        h_sequence(h_uint8_const(WRITE_MULTIPLE_COILS), modbus_write_multiple_request),
        h_sequence(h_uint8_const(WRITE_MULTIPLE_REGISTERS), modbus_write_multiple_request),
        NULL
    );

    modbus_response = h_choice(
        h_sequence(h_uint8_const(READ_COILS), modbus_read_response),
        h_sequence(h_uint8_const(READ_DISCRETE_INPUTS), modbus_read_response),
        h_sequence(h_uint8_const(READ_HOLDING_REGISTERS), modbus_read_response),
        h_sequence(h_uint8_const(READ_INPUT_REGISTERS), modbus_read_response),
        h_sequence(h_uint8_const(WRITE_SINGLE_COIL), modbus_write_response),
        h_sequence(h_uint8_const(WRITE_SINGLE_REGISTER), modbus_write_response),
        h_sequence(h_uint8_const(WRITE_MULTIPLE_COILS), modbus_write_response),
        h_sequence(h_uint8_const(WRITE_MULTIPLE_REGISTERS), modbus_write_response),
        h_sequence(h_uint8_range(0x80, 0xBF), modbus_exception_response),
        NULL
    );

    modbus_pdu = h_choice(modbus_request, modbus_response, NULL);
}

int main(int argc, char **argv) {
    init_modbus_parsers();
    // Further code to use modbus_pdu parser
}