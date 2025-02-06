#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

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

typedef struct {
    uint8_t slave_address;
    ModbusFunctionCode function_code;
    struct {
        uint16_t starting_address;
        uint16_t quantity;
        uint8_t byte_count;
        uint8_t* data;
    } request;
    uint16_t crc;
} ModbusFrame;

static HParser* modbus_function_code_parser() {
    return h_choice(
        h_literal(READ_COILS),
        h_literal(READ_DISCRETE_INPUTS),
        h_literal(READ_HOLDING_REGISTERS),
        h_literal(READ_INPUT_REGISTERS),
        h_literal(WRITE_SINGLE_COIL),
        h_literal(WRITE_SINGLE_REGISTER),
        h_literal(WRITE_MULTIPLE_COILS),
        h_literal(WRITE_MULTIPLE_REGISTERS),
        h_literal(MASK_WRITE_REGISTER),
        h_literal(READ_WRITE_MULTIPLE_REGISTERS),
        NULL
    );
}

static const HParsedToken* parse_modbus_frame(void* context) {
    HParser* slave_address = h_uint8();
    HParser* function_code = modbus_function_code_parser();
    
    HParser* starting_address = h_uint16();
    HParser* quantity = h_uint16();
    HParser* byte_count = h_uint8();
    
    HParser* data_parser = h_repeat_n(h_uint8(), 1);
    
    HParser* crc = h_uint16();
    
    HParser* frame_parser = h_sequence(
        slave_address,
        function_code,
        starting_address,
        quantity,
        byte_count,
        data_parser,
        crc,
        NULL
    );
    
    HParseResult* result = h_parse(frame_parser, NULL, 0);
    return result ? result->ast : NULL;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
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
        perror("File read error");
        free(buffer);
        return 1;
    }
    
    HParser* modbus_parser = h_indirect();
    h_bind_indirect(modbus_parser, modbus_function_code_parser());
    
    HParseResult* parsed_frame = h_parse(modbus_parser, buffer, file_size);
    
    if (parsed_frame && parsed_frame->ast) {
        printf("Modbus frame parsed successfully\n");
    } else {
        printf("Parsing failed\n");
    }
    
    h_parse_result_free(parsed_frame);
    free(buffer);
    return 0;
}