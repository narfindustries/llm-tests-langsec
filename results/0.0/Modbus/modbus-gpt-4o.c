#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

HParser *modbus_pdu_parser();
HParser *modbus_rtu_parser();
HParser *modbus_tcp_parser();

HParser *modbus_pdu_parser() {
    HParser *function_code = h_uint8();
    HParser *data = h_many(h_uint8()); // Use h_many for variable length data
    return h_sequence(function_code, data, NULL);
}

HParser *modbus_rtu_parser() {
    HParser *address = h_uint8();
    HParser *pdu = modbus_pdu_parser();
    HParser *crc = h_uint16(); // Assuming little-endian by default
    return h_sequence(address, pdu, crc, NULL);
}

HParser *modbus_tcp_parser() {
    HParser *transaction_id = h_uint16(); // Assuming big-endian by default
    HParser *protocol_id = h_uint16();
    HParser *length = h_uint16();
    HParser *unit_id = h_uint8();
    HParser *pdu = modbus_pdu_parser();
    return h_sequence(transaction_id, protocol_id, length, unit_id, pdu, NULL);
}

void parse_modbus_file(const char *filename, HParser *parser) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(parser, buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <rtu|tcp> <filename>\n", argv[0]);
        return EXIT_FAILURE;
    }

    HParser *parser = NULL;
    if (strcmp(argv[1], "rtu") == 0) {
        parser = modbus_rtu_parser();
    } else if (strcmp(argv[1], "tcp") == 0) {
        parser = modbus_tcp_parser();
    } else {
        fprintf(stderr, "Invalid mode: %s. Use 'rtu' or 'tcp'.\n", argv[1]);
        return EXIT_FAILURE;
    }

    parse_modbus_file(argv[2], parser);
    return EXIT_SUCCESS;
}