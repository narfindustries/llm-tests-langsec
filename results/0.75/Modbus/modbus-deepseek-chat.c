#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the Modbus PDU structure
typedef struct {
    uint8_t function_code;
    uint16_t starting_address;
    uint16_t quantity;
} ModbusPDU;

// Define the Modbus ADU structure
typedef struct {
    uint8_t transaction_id;
    uint8_t protocol_id;
    uint16_t length;
    ModbusPDU pdu;
} ModbusADU;

// Parser for Modbus PDU
HParser *modbus_pdu_parser() {
    return h_sequence(
        h_uint8(),  // function_code
        h_uint16(), // starting_address
        h_uint16(), // quantity
        NULL
    );
}

// Parser for Modbus ADU
HParser *modbus_adu_parser() {
    return h_sequence(
        h_uint8(),  // transaction_id
        h_uint8(),  // protocol_id
        h_uint16(), // length
        h_embed(modbus_pdu_parser()), // pdu
        NULL
    );
}

// Main function to parse Modbus ADU
int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
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

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = modbus_adu_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}