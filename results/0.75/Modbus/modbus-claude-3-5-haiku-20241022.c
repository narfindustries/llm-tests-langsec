#include <hammer/hammer.h>
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

typedef struct {
    uint8_t server_address;
    ModbusFunctionCode function_code;
    HParsedToken* data;
    uint16_t crc;
} ModbusADU;

static HParser* modbus_server_address;
static HParser* modbus_function_code;
static HParser* modbus_data_parser;
static HParser* modbus_crc;
static HParser* modbus_adu_parser;

HParser* create_modbus_parser() {
    modbus_server_address = h_uint8();
    
    modbus_function_code = h_choice(
        h_token((uint8_t[]){READ_COILS}, 1),
        h_token((uint8_t[]){READ_DISCRETE_INPUTS}, 1),
        h_token((uint8_t[]){READ_HOLDING_REGISTERS}, 1),
        h_token((uint8_t[]){READ_INPUT_REGISTERS}, 1),
        h_token((uint8_t[]){WRITE_SINGLE_COIL}, 1),
        h_token((uint8_t[]){WRITE_SINGLE_REGISTER}, 1),
        h_token((uint8_t[]){WRITE_MULTIPLE_COILS}, 1),
        h_token((uint8_t[]){WRITE_MULTIPLE_REGISTERS}, 1),
        h_token((uint8_t[]){MASK_WRITE_REGISTER}, 1),
        h_token((uint8_t[]){READ_WRITE_MULTIPLE_REGISTERS}, 1),
        NULL
    );

    modbus_data_parser = h_many(h_uint8());
    
    modbus_crc = h_uint16();

    modbus_adu_parser = h_sequence(
        modbus_server_address,
        modbus_function_code,
        modbus_data_parser,
        modbus_crc,
        NULL
    );

    return modbus_adu_parser;
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

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = create_modbus_parser();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        ModbusADU* adu = (ModbusADU*)result->ast;
        printf("Server Address: %d\n", adu->server_address);
        printf("Function Code: 0x%02X\n", adu->function_code);
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}