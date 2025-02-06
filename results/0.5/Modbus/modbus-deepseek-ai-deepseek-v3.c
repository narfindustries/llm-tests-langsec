#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t transaction_id;
    uint16_t protocol_id;
    uint16_t length;
    uint8_t unit_id;
    uint8_t function_code;
    uint8_t *data;
    uint16_t checksum;
} ModbusFrame;

HParser *modbus_parser() {
    HParser *transaction_id = h_uint16();
    HParser *protocol_id = h_uint16();
    HParser *length = h_uint16();
    HParser *unit_id = h_uint8();
    HParser *function_code = h_uint8();
    HParser *data = h_many(h_uint8());
    HParser *checksum = h_uint16();

    return h_sequence(
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data,
        checksum,
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = modbus_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        ModbusFrame *frame = (ModbusFrame *)result->ast;
        printf("Transaction ID: %04X\n", frame->transaction_id);
        printf("Protocol ID: %04X\n", frame->protocol_id);
        printf("Length: %04X\n", frame->length);
        printf("Unit ID: %02X\n", frame->unit_id);
        printf("Function Code: %02X\n", frame->function_code);
        printf("Data: ");
        for (int i = 0; i < frame->length - 2; i++) {
            printf("%02X ", frame->data[i]);
        }
        printf("\nChecksum: %04X\n", frame->checksum);
        free(result->ast);
        free(result);
    } else {
        fprintf(stderr, "Failed to parse Modbus frame\n");
    }

    free(buffer);
    return 0;
}