#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define Modbus PDU types
typedef enum {
    MODBUS_READ_COILS = 0x01,
    MODBUS_READ_DISCRETE_INPUTS = 0x02,
    MODBUS_READ_HOLDING_REGISTERS = 0x03,
    MODBUS_READ_INPUT_REGISTERS = 0x04,
    MODBUS_WRITE_SINGLE_COIL = 0x05,
    MODBUS_WRITE_SINGLE_REGISTER = 0x06,
    MODBUS_WRITE_MULTIPLE_COILS = 0x0F,
    MODBUS_WRITE_MULTIPLE_REGISTERS = 0x10
} ModbusFunctionCode;

// Define Modbus PDU parsers
HParser *modbus_pdu_parser() {
    return h_sequence(
        h_uint8(),  // Function Code
        h_uint16(), // Starting Address
        h_uint16(), // Quantity/Value
        NULL
    );
}

// Define Modbus ADU parsers
HParser *modbus_adu_parser() {
    return h_sequence(
        h_uint8(),  // Transaction ID
        h_uint8(),  // Protocol ID
        h_uint16(), // Length
        h_uint8(),  // Unit ID
        modbus_pdu_parser(), // PDU
        NULL
    );
}

// Main function
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(modbus_adu_parser(), data, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse Modbus ADU\n");
        free(data);
        return 1;
    }

    // Print parsed result
    printf("Transaction ID: %d\n", result->ast->seq->elements[0]->uint);
    printf("Protocol ID: %d\n", result->ast->seq->elements[1]->uint);
    printf("Length: %d\n", result->ast->seq->elements[2]->uint);
    printf("Unit ID: %d\n", result->ast->seq->elements[3]->uint);
    printf("Function Code: %d\n", result->ast->seq->elements[4]->seq->elements[0]->uint);
    printf("Starting Address: %d\n", result->ast->seq->elements[4]->seq->elements[1]->uint);
    printf("Quantity/Value: %d\n", result->ast->seq->elements[4]->seq->elements[2]->uint);

    h_parse_result_free(result);
    free(data);

    return 0;
}