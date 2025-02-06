#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint8_t byte;
typedef uint16_t word;

static hammer_parser_t modbus_address = hammer_uint8_t();
static hammer_parser_t modbus_function_code = hammer_uint8_t();
static hammer_parser_t modbus_starting_address = hammer_uint16_t_be();
static hammer_parser_t modbus_quantity = hammer_uint16_t_be();
static hammer_parser_t modbus_data = hammer_bytes(0);
static hammer_parser_t modbus_crc = hammer_uint16_t_be();

static hammer_parser_t parse_modbus_pdu(hammer_context_t *ctx) {
    return hammer_and_then(ctx,
                          hammer_sequence(ctx,
                                          modbus_address,
                                          modbus_function_code,
                                          modbus_starting_address,
                                          modbus_quantity,
                                          modbus_data,
                                          modbus_crc),
                          hammer_check_eof(ctx));
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    byte *buffer = (byte *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, fsize, fp) != fsize) {
        perror("Error reading file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    hammer_context_t ctx;
    hammer_context_init(&ctx, buffer, fsize);

    hammer_result_t result = hammer_parse(&ctx, parse_modbus_pdu);

    if (result.status == HAMMER_STATUS_SUCCESS) {
        printf("Modbus PDU parsed successfully:\n");
        printf("  Address: 0x%02X\n", *(uint8_t *)result.value[0]);
        printf("  Function Code: 0x%02X\n", *(uint8_t *)result.value[1]);
        printf("  Starting Address: 0x%04X\n", *(uint16_t *)result.value[2]);
        printf("  Quantity: 0x%04X\n", *(uint16_t *)result.value[3]);
        size_t data_len = hammer_bytes_len(result.value[4]);
        printf("  Data Length: %zu\n", data_len);
        printf("  CRC: 0x%04X\n", *(uint16_t *)result.value[5]);

        hammer_free_result(&result);
    } else {
        fprintf(stderr, "Error parsing Modbus PDU: %s\n", hammer_error_message(result.status));
    }

    free(buffer);
    return 0;
}
