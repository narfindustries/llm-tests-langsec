#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef uint16_t uint16;
typedef uint8_t uint8;

hammer_parser_t* modbus_pdu_parser() {
    hammer_parser_t* parser = hammer_seq(
        hammer_uint16("transaction_id"),
        hammer_uint16("protocol_id"),
        hammer_uint16("length"),
        hammer_uint8("unit_id"),
        hammer_uint8("function_code"),
        hammer_choice(
            hammer_map(
                hammer_seq(
                    hammer_uint16("starting_address"),
                    hammer_uint16("quantity")
                ),
                hammer_ignore
            ),
            hammer_map(
                hammer_seq(
                    hammer_uint16("starting_address"),
                    hammer_uint16("quantity"),
                    hammer_bytes("data", 0)
                ),
                hammer_ignore
            ),
            hammer_map(
                hammer_seq(
                    hammer_uint16("starting_address"),
                    hammer_uint16("quantity"),
                    hammer_uint16("value")
                ),
                hammer_ignore
            ),
            hammer_map(
                hammer_uint8("exception_code"),
                hammer_ignore
            ),
            hammer_fail("Invalid function code")
        )
    );
    return parser;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8* buffer = (uint8*)malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser_t* parser = modbus_pdu_parser();
    hammer_result_t result = hammer_parse(parser, buffer, fsize);

    if (result.success) {
        printf("Modbus PDU parsed successfully!\n");
    } else {
        fprintf(stderr, "Error parsing Modbus PDU: %s\n", result.error);
    }

    hammer_free(parser);
    free(buffer);
    return 0;
}
