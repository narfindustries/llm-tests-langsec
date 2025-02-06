#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef enum {
    READ_COILS = 0x01,
    READ_DISCRETE_INPUTS = 0x02,
    READ_HOLDING_REGISTERS = 0x03,
    READ_INPUT_REGISTERS = 0x04,
    WRITE_SINGLE_COIL = 0x05,
    WRITE_SINGLE_REGISTER = 0x06,
    WRITE_MULTIPLE_COILS = 0x0F,
    WRITE_MULTIPLE_REGISTERS = 0x10,
    MASK_WRITE_REGISTER = 0x16,
    READ_WRITE_MULTIPLE_REGISTERS = 0x17
} ModbusFunctionCode;

typedef enum {
    ILLEGAL_FUNCTION = 0x01,
    ILLEGAL_DATA_ADDRESS = 0x02,
    ILLEGAL_DATA_VALUE = 0x03,
    SLAVE_DEVICE_FAILURE = 0x04,
    ACKNOWLEDGE = 0x05,
    SLAVE_DEVICE_BUSY = 0x06,
    NEGATIVE_ACKNOWLEDGE = 0x07,
    MEMORY_PARITY_ERROR = 0x08
} ModbusExceptionCode;

typedef struct {
    uint8_t slave_address;
    ModbusFunctionCode function_code;
    uint16_t starting_address;
    uint16_t quantity;
    uint8_t byte_count;
    uint8_t* data;
    uint16_t crc;
} ModbusFrame;

static HParsedToken* parse_slave_address(const HParseResult* p, void* user_data) {
    HArena* arena = h_new_arena();
    return h_make_uint(arena, *(uint8_t*)p->ast);
}

static HParsedToken* parse_function_code(const HParseResult* p, void* user_data) {
    HArena* arena = h_new_arena();
    return h_make_uint(arena, *(uint8_t*)p->ast);
}

static HParsedToken* parse_starting_address(const HParseResult* p, void* user_data) {
    HArena* arena = h_new_arena();
    return h_make_uint(arena, *(uint16_t*)p->ast);
}

static HParsedToken* parse_quantity(const HParseResult* p, void* user_data) {
    HArena* arena = h_new_arena();
    return h_make_uint(arena, *(uint16_t*)p->ast);
}

static HParsedToken* parse_byte_count(const HParseResult* p, void* user_data) {
    HArena* arena = h_new_arena();
    return h_make_uint(arena, *(uint8_t*)p->ast);
}

static HParsedToken* parse_data(const HParseResult* p, void* user_data) {
    HArena* arena = h_new_arena();
    return h_make_seq(arena);
}

static HParsedToken* parse_crc(const HParseResult* p, void* user_data) {
    HArena* arena = h_new_arena();
    return h_make_uint(arena, *(uint16_t*)p->ast);
}

HParser* modbus_parser() {
    HParser* slave_address = h_action(h_uint8(), parse_slave_address, NULL);
    HParser* function_code = h_action(h_uint8(), parse_function_code, NULL);
    HParser* starting_address = h_action(h_uint16_big(), parse_starting_address, NULL);
    HParser* quantity = h_action(h_uint16_big(), parse_quantity, NULL);
    HParser* byte_count = h_action(h_uint8(), parse_byte_count, NULL);
    HParser* data = h_action(h_many(h_uint8()), parse_data, NULL);
    HParser* crc = h_action(h_uint16_big(), parse_crc, NULL);

    HParser* modbus_frame = h_sequence(
        slave_address,
        function_code,
        starting_address,
        quantity,
        byte_count,
        data,
        crc,
        NULL
    );

    return modbus_frame;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <modbus_binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("Error reading file");
        free(buffer);
        return 1;
    }

    HParser* parser = modbus_parser();
    HParseResult* result = h_parse(parser, buffer, bytes_read);

    if (result) {
        printf("Modbus frame parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Modbus frame parsing failed\n");
    }

    h_parser_free(parser);
    free(buffer);
    return 0;
}