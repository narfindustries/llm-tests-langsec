#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// HL7 v2.x message segments
HParser *hl7_msh;
HParser *hl7_evn;
HParser *hl7_pid;
HParser *hl7_pvx;
HParser *hl7_obx;
HParser *hl7_msg;

// HL7 field separator and encoding characters
static const uint8_t HL7_FIELD_SEP = '|';
static const uint8_t HL7_COMP_SEP = '^';
static const uint8_t HL7_REP_SEP = '~';
static const uint8_t HL7_ESCAPE_CHAR = '\\';
static const uint8_t HL7_SUBCOMP_SEP = '&';

// HL7 data types
HParser *hl7_field;
HParser *hl7_component;
HParser *hl7_repeated;
HParser *hl7_subcomponent;

void init_hl7_parsers() {
    hl7_field = h_ch(HL7_FIELD_SEP);
    hl7_component = h_ch(HL7_COMP_SEP);
    hl7_repeated = h_ch(HL7_REP_SEP);
    hl7_subcomponent = h_ch(HL7_SUBCOMP_SEP);

    hl7_msh = h_sequence(h_ch('M'), h_ch('S'), h_ch('H'), hl7_field, NULL);
    hl7_evn = h_sequence(h_ch('E'), h_ch('V'), h_ch('N'), hl7_field, NULL);
    hl7_pid = h_sequence(h_ch('P'), h_ch('I'), h_ch('D'), hl7_field, NULL);
    hl7_pvx = h_sequence(h_ch('P'), h_ch('V'), h_ch('X'), hl7_field, NULL);
    hl7_obx = h_sequence(h_ch('O'), h_ch('B'), h_ch('X'), hl7_field, NULL);

    hl7_msg = h_choice(hl7_msh, hl7_evn, hl7_pid, hl7_pvx, hl7_obx, NULL);
}

int parse_hl7(const uint8_t *input, size_t length) {
    HParseResult *result = h_parse(hl7_msg, input, length);
    if (result) {
        printf("HL7 message parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 4);
        h_parse_result_free(result);
        return 0;
    } else {
        printf("Failed to parse HL7 message.\n");
        return 1;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, length, file);
    fclose(file);

    init_hl7_parsers();
    int result = parse_hl7(data, length);
    free(data);

    return result;
}