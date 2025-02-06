#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function prototypes for parsers
static HParser *uint16();
static HParser *uint8();
static HParser *mbap();
static HParser *modbus_pdu();
static HParser *modbus_frame();

// Parser for a 16-bit unsigned integer
static HParser *uint16() {
    return h_uint16();
}

// Parser for an 8-bit unsigned integer
static HParser *uint8() {
    return h_uint8();
}

// Modbus Application Protocol Header (MBAP)
static HParser *mbap() {
    return h_sequence(uint16(),  // Transaction Identifier
                      uint16(),  // Protocol Identifier
                      uint16(),  // Length
                      uint8(),   // Unit Identifier
                      NULL);
}

// Parser for Modbus function code with its respective payload
static HParser *modbus_pdu() {
    return h_sequence(uint8(),   // Function Code
                      h_many(h_uint8()),  // Data Field (variable bytes)
                      NULL);
}

// Combined MBAP and PDU
static HParser *modbus_frame() {
    return h_sequence(mbap(), modbus_pdu(), NULL);
}

// Read contents of a file into a buffer
unsigned char *read_file(const char *filename, size_t *length) {
    FILE *file = fopen(filename, "rb");
    if (!file) return NULL;

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(size);
    fread(data, 1, size, file);
    fclose(file);

    *length = size;
    return data;
}

// Main function
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <modbus_binary_file>\n", argv[0]);
        return 1;
    }

    size_t len;
    unsigned char *data = read_file(argv[1], &len);
    if (!data) {
        fprintf(stderr, "Error reading file.\n");
        return 1;
    }

    HParser *modbusParser = modbus_frame();
    HParseResult *result = h_parse(modbusParser, data, len);
    if (result) {
        printf("Parse success.\n");
        h_pprint(stdout, result->ast, 0, 4);
        h_parse_result_free(result);
    } else {
        printf("Parse failed.\n");
    }

    free(data);
    return 0;
}