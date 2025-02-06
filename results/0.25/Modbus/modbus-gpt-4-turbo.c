#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function Prototypes
static void parse_modbus(const char *filename);
static void print_hex(const uint8_t *data, size_t length);

// Parsers
static HParser *modbus_tcp;
static HParser *modbus_pdu;
static HParser *modbus_function;
static HParser *modbus_data;

void build_modbus_parsers() {
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();

    // MBAP Header
    HParser *transaction_id = uint16;
    HParser *protocol_id = uint16;
    HParser *length = uint16;
    HParser *unit_id = uint8;

    // Function Code and Data
    modbus_function = uint8;
    modbus_data = h_length_value(uint16, h_bytes(1));  // Use h_bytes for variable length data

    // Modbus PDU
    modbus_pdu = h_sequence(modbus_function, modbus_data, NULL);

    // Modbus TCP Frame (MBAP + PDU)
    modbus_tcp = h_sequence(transaction_id, protocol_id, length, unit_id, modbus_pdu, NULL);
}

void parse_modbus(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        return;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory for file buffer\n");
        fclose(file);
        return;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(modbus_tcp, buffer, file_size);
    if (result) {
        printf("Parse successful!\n");
        print_hex(buffer, file_size);
    } else {
        printf("Parse failed!\n");
    }

    free(buffer);
}

void print_hex(const uint8_t *data, size_t length) {
    for (size_t i = 0; i < length; i++) {
        printf("%02X ", data[i]);
    }
    printf("\n");
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <modbus_binary_file>\n", argv[0]);
        return 1;
    }

    build_modbus_parsers();
    parse_modbus(argv[1]);

    return 0;
}