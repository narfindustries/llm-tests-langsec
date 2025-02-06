#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))

typedef struct {
    uint8_t address;
    uint8_t function_code;
    uint16_t starting_address;
    uint16_t quantity;
    uint8_t byte_count;
    uint8_t* data;
    uint16_t crc;
} modbus_message_t;

typedef struct {
    uint8_t address;
    uint8_t function_code;
    uint16_t starting_address;
    uint16_t quantity;
    uint8_t byte_count;
    uint8_t* data;
} modbus_request_t;

typedef struct {
    uint8_t address;
    uint8_t function_code;
    uint8_t exception_code;
} modbus_exception_response_t;

typedef struct {
    uint8_t address;
    uint8_t function_code;
    uint16_t starting_address;
    uint16_t quantity;
    uint8_t byte_count;
    uint8_t* data;
} modbus_response_t;

typedef enum {
    HAMMER_OK,
    HAMMER_ERROR
} hammer_status_t;

typedef struct {
    hammer_status_t status;
    void* value;
} hammer_result_t;

typedef struct hammer_parser {
    int (*parse)(struct hammer_parser*, uint8_t**, size_t*);
    void* data;
} hammer_parser_t;

int hammer_byte_parse(hammer_parser_t* parser, uint8_t** data, size_t* size) {
    if (*size < 1) {
        return 0;
    }
    uint8_t byte = **data;
    (*data)++;
    (*size)--;
    return byte;
}

hammer_parser_t* hammer_byte() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parse = hammer_byte_parse;
    return parser;
}

int hammer_word_be_parse(hammer_parser_t* parser, uint8_t** data, size_t* size) {
    if (*size < 2) {
        return 0;
    }
    uint16_t word = ((*data)[0] << 8) | ((*data)[1]);
    (*data) += 2;
    (*size) -= 2;
    return word;
}

hammer_parser_t* hammer_word_be() {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parse = hammer_word_be_parse;
    return parser;
}

int hammer_bytes_parse(hammer_parser_t* parser, uint8_t** data, size_t* size) {
    hammer_parser_t* length_parser = (hammer_parser_t*) parser->data;
    int length = length_parser->parse(length_parser, data, size);
    if (length <= 0) {
        return 0;
    }
    if (*size < length) {
        return 0;
    }
    uint8_t* bytes = *data;
    (*data) += length;
    (*size) -= length;
    return (intptr_t) bytes;
}

hammer_parser_t* hammer_bytes(hammer_parser_t* length_parser) {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parse = hammer_bytes_parse;
    parser->data = length_parser;
    return parser;
}

int hammer_struct_parse(hammer_parser_t* parser, uint8_t** data, size_t* size) {
    hammer_parser_t** parsers = (hammer_parser_t**) parser->data;
    int values[7];
    for (int i = 0; i < 7; i++) {
        values[i] = parsers[i]->parse(parsers[i], data, size);
        if (values[i] <= 0) {
            return 0;
        }
    }
    modbus_message_t* message = malloc(sizeof(modbus_message_t));
    message->address = (uint8_t) values[0];
    message->function_code = (uint8_t) values[1];
    message->starting_address = (uint16_t) values[2];
    message->quantity = (uint16_t) values[3];
    message->byte_count = (uint8_t) values[4];
    message->data = (uint8_t*) (intptr_t) values[5];
    message->crc = (uint16_t) values[6];
    return (intptr_t) message;
}

hammer_parser_t* hammer_struct(hammer_parser_t* parser1, hammer_parser_t* parser2, hammer_parser_t* parser3, hammer_parser_t* parser4, hammer_parser_t* parser5, hammer_parser_t* parser6, hammer_parser_t* parser7) {
    hammer_parser_t* parser = malloc(sizeof(hammer_parser_t));
    parser->parse = hammer_struct_parse;
    parser->data = malloc(sizeof(hammer_parser_t*) * 7);
    ((hammer_parser_t**)parser->data)[0] = parser1;
    ((hammer_parser_t**)parser->data)[1] = parser2;
    ((hammer_parser_t**)parser->data)[2] = parser3;
    ((hammer_parser_t**)parser->data)[3] = parser4;
    ((hammer_parser_t**)parser->data)[4] = parser5;
    ((hammer_parser_t**)parser->data)[5] = parser6;
    ((hammer_parser_t**)parser->data)[6] = parser7;
    return parser;
}

hammer_result_t hammer_parse(hammer_parser_t* parser, uint8_t* data, size_t size) {
    uint8_t* original_data = data;
    size_t original_size = size;
    intptr_t result = parser->parse(parser, &data, &size);
    if (result <= 0) {
        hammer_result_t hammer_result;
        hammer_result.status = HAMMER_ERROR;
        hammer_result.value = NULL;
        return hammer_result;
    }
    hammer_result_t hammer_result;
    hammer_result.status = HAMMER_OK;
    hammer_result.value = (void*) result;
    return hammer_result;
}

char* hammer_error_message(hammer_status_t status) {
    if (status == HAMMER_OK) {
        return "No error";
    } else {
        return "Unknown error";
    }
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Could not open file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Out of memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    hammer_parser_t* address_parser = hammer_byte();
    hammer_parser_t* function_code_parser = hammer_byte();
    hammer_parser_t* starting_address_parser = hammer_word_be();
    hammer_parser_t* quantity_parser = hammer_word_be();
    hammer_parser_t* byte_count_parser = hammer_byte();
    hammer_parser_t* data_parser = hammer_bytes(byte_count_parser);
    hammer_parser_t* crc_parser = hammer_word_be();

    hammer_parser_t* parser = hammer_struct(address_parser, function_code_parser, starting_address_parser, quantity_parser, byte_count_parser, data_parser, crc_parser);

    hammer_result_t result = hammer_parse(parser, data, file_size);

    if (result.status == HAMMER_OK) {
        modbus_message_t* message = (modbus_message_t*) result.value;
        printf("Address: %u\n", message->address);
        printf("Function Code: %u\n", message->function_code);
        printf("Starting Address: %u\n", message->starting_address);
        printf("Quantity: %u\n", message->quantity);
        printf("Byte Count: %u\n", message->byte_count);
        printf("Data: ");
        for (size_t i = 0; i < message->byte_count; i++) {
            printf("%02x ", message->data[i]);
        }
        printf("\n");
        printf("CRC: %u\n", message->crc);
    } else {
        printf("Parse error: %s\n", hammer_error_message(result.status));
    }

    free(data);

    return 0;
}