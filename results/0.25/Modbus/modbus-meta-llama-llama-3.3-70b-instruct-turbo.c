#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define MODBUS_MAX_DATA_LENGTH 256

typedef enum {
    MODBUS_FUNCTION_READ_COIL_STATUS = 0x01,
    MODBUS_FUNCTION_READ_INPUT_STATUS = 0x02,
    MODBUS_FUNCTION_READ_HOLDING_REGISTERS = 0x03,
    MODBUS_FUNCTION_READ_INPUT_REGISTERS = 0x04,
    MODBUS_FUNCTION_WRITE_SINGLE_COIL = 0x05,
    MODBUS_FUNCTION_WRITE_SINGLE_REGISTER = 0x06,
    MODBUS_FUNCTION_WRITE_MULTIPLE_COILS = 0x0F,
    MODBUS_FUNCTION_WRITE_MULTIPLE_REGISTERS = 0x10,
    MODBUS_FUNCTION_REPORT_SLAVE_ID = 0x11,
    MODBUS_FUNCTION_READ_FILE_RECORD = 0x14,
    MODBUS_FUNCTION_WRITE_FILE_RECORD = 0x15,
    MODBUS_FUNCTION_MASK_WRITE_REGISTER = 0x16,
    MODBUS_FUNCTION_READ_WRITE_MULTIPLE_REGISTERS = 0x17,
    MODBUS_FUNCTION_READ_FIFO_QUEUE = 0x18,
    MODBUS_FUNCTION_ENCAPSULATED_INTERFACE_TRANSPORT = 0x2B
} modbus_function_code_t;

typedef struct {
    uint8_t address;
    uint8_t function_code;
    uint8_t* data;
    size_t data_length;
} modbus_message_t;

typedef struct {
    uint8_t address;
    uint8_t function_code;
    uint8_t error_code;
} modbus_exception_response_t;

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* input_file = fopen(argv[1], "rb");
    if (!input_file) {
        printf("Error opening input file\n");
        return 1;
    }

    fseek(input_file, 0, SEEK_END);
    size_t input_length = ftell(input_file);
    rewind(input_file);

    uint8_t* input_data = malloc(input_length);
    if (!input_data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t bytes_read = fread(input_data, 1, input_length, input_file);
    if (bytes_read != input_length) {
        printf("Error reading input file\n");
        return 1;
    }

    fclose(input_file);

    HParser* address_parser = h_uint8();
    HParser* function_code_parser = h_uint8();
    HParser* data_parser = h_repeated(h_uint8(), MODBUS_MAX_DATA_LENGTH);
    HParser* crc_parser = h_uint16();

    HParser* message_parser = h_tupled(address_parser, function_code_parser, data_parser, crc_parser);

    HParser* exception_response_parser = h_struct(address_parser, function_code_parser, h_uint8());

    HParseResult* result = h_parse(message_parser, input_data, input_length);
    if (h_result_is_ok(result)) {
        modbus_message_t* message = (modbus_message_t*)h_result_get_value(result);
        printf("Address: %u\n", message->address);
        printf("Function Code: %u\n", message->function_code);
        printf("Data Length: %zu\n", message->data_length);
        for (size_t i = 0; i < message->data_length; i++) {
            printf("Data[%zu]: %u\n", i, message->data[i]);
        }
    } else {
        result = h_parse(exception_response_parser, input_data, input_length);
        if (h_result_is_ok(result)) {
            modbus_exception_response_t* exception_response = (modbus_exception_response_t*)h_result_get_value(result);
            printf("Address: %u\n", exception_response->address);
            printf("Function Code: %u\n", exception_response->function_code);
            printf("Error Code: %u\n", exception_response->error_code);
        } else {
            printf("Error parsing input data\n");
        }
    }

    free(input_data);
    return 0;
}