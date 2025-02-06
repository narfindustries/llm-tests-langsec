#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// HL7 Message Delimiters
#define FIELD_SEPARATOR '|'
#define COMPONENT_SEPARATOR '^'
#define REPEAT_SEPARATOR '~'
#define ESCAPE_CHARACTER '\\'
#define SUBCOMPONENT_SEPARATOR '&'
#define SEGMENT_TERMINATOR '\r'

// Helper function prototypes
HParser *hl7_field();
HParser *hl7_component();
HParser *optional_repetitions(HParser *p);

// HL7 Segment Parsers
HParser *segment_MSH();
HParser *segment_PID();
HParser *segment_OBR();
HParser *segment_ORC();
HParser *segment_OBX();

// Main HL7 Message Parser
HParser *hl7_message();

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t len = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *data = malloc(len + 1);
    if (!data) {
        perror("Memory allocation failed");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(data, 1, len, fp);
    fclose(fp);
    data[len] = '\0'; // Null-terminate the data for parsing

    HParser *parser = hl7_message();
    HParseResult *result = h_parse(parser, (const uint8_t *)data, len);
    if (result) {
        printf("HL7 message parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse HL7 message.\n");
    }

    h_parse_result_free(result);
    free(data);

    return EXIT_SUCCESS;
}

HParser *hl7_field() {
    return h_sepBy1(h_ch(ESCAPE_CHARACTER), h_ch(FIELD_SEPARATOR));
}

HParser *hl7_component() {
    return h_sepBy1(h_any(), h_ch(ESCAPE_CHARACTER));
}

HParser *optional_repetitions(HParser *p) {
    return h_optional(h_sepBy(p, h_ch(REPEAT_SEPARATOR)));
}

HParser *segment_MSH() {
    return h_sequence(h_string("MSH", 3), h_ch(FIELD_SEPARATOR), hl7_field(), NULL);
}

HParser *segment_PID() {
    return h_sequence(h_string("PID", 3), h_ch(FIELD_SEPARATOR), hl7_field(), NULL);
}

HParser *segment_OBR() {
    return h_sequence(h_string("OBR", 3), h_ch(FIELD_SEPARATOR), hl7_field(), NULL);
}

HParser *segment_ORC() {
    return h_sequence(h_string("ORC", 3), h_ch(FIELD_SEPARATOR), hl7_field(), NULL);
}

HParser *segment_OBX() {
    return h_sequence(h_string("OBX", 3), h_ch(FIELD_SEPARATOR), hl7_field(), NULL);
}

HParser *hl7_message() {
    return h_many(h_sequence(
        h_choice(segment_MSH(), segment_PID(), segment_OBR(), segment_ORC(), segment_OBX(), NULL),
        h_ch(SEGMENT_TERMINATOR),
        NULL
    ));
}