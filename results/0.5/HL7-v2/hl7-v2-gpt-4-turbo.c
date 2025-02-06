#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for basic HL7 data types
HParser *hl7_alphanumeric;
HParser *hl7_numeric;
HParser *hl7_datetime;

// Define parsers for HL7 fields
HParser *msh_field;
HParser *pid_field;
HParser *orc_field;
HParser *obr_field;
HParser *obx_field;

// Define parsers for HL7 segments
HParser *msh_segment;
HParser *pid_segment;
HParser *orc_segment;
HParser *obr_segment;
HParser *obx_segment;

// Define parser for an HL7 message
HParser *hl7_message;

void init_parsers() {
    hl7_alphanumeric = h_token((const uint8_t *)"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", 62);
    hl7_numeric = h_token((const uint8_t *)"0123456789", 10);
    hl7_datetime = h_sequence(
        h_repeat_n(hl7_numeric, 4), h_ch('-'),
        h_repeat_n(hl7_numeric, 2), h_ch('-'),
        h_repeat_n(hl7_numeric, 2), h_ch('T'),
        h_repeat_n(hl7_numeric, 2), h_ch(':'),
        h_repeat_n(hl7_numeric, 2), h_ch(':'),
        h_repeat_n(hl7_numeric, 2), NULL
    );

    msh_field = h_sequence(hl7_alphanumeric, NULL);
    pid_field = h_sequence(hl7_alphanumeric, NULL);
    orc_field = h_sequence(hl7_alphanumeric, NULL);
    obr_field = h_sequence(hl7_alphanumeric, NULL);
    obx_field = h_sequence(hl7_alphanumeric, NULL);

    msh_segment = h_sequence(h_ch('M'), h_ch('S'), h_ch('H'), h_ch('|'), h_many(msh_field), h_ch('\r'), NULL);
    pid_segment = h_sequence(h_ch('P'), h_ch('I'), h_ch('D'), h_ch('|'), h_many(pid_field), h_ch('\r'), NULL);
    orc_segment = h_sequence(h_ch('O'), h_ch('R'), h_ch('C'), h_ch('|'), h_many(orc_field), h_ch('\r'), NULL);
    obr_segment = h_sequence(h_ch('O'), h_ch('B'), h_ch('R'), h_ch('|'), h_many(obr_field), h_ch('\r'), NULL);
    obx_segment = h_sequence(h_ch('O'), h_ch('B'), h_ch('X'), h_ch('|'), h_many(obx_field), h_ch('\r'), NULL);

    hl7_message = h_sequence(msh_segment, h_many(pid_segment), h_many(orc_segment), h_many(obr_segment), h_many(obx_segment), NULL);
}

void parse_hl7_message(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        return;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (!data) {
        fprintf(stderr, "Failed to allocate memory for file data\n");
        fclose(file);
        return;
    }

    fread(data, 1, length, file);
    fclose(file);

    HParseResult *result = h_parse(hl7_message, data, length);
    if (result) {
        printf("HL7 message parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse HL7 message.\n");
    }

    free(data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_message_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    init_parsers();
    parse_hl7_message(argv[1]);
    return EXIT_SUCCESS;
}