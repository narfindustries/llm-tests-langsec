#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for basic HL7 data types
HParser *hl7_alphanumeric;
HParser *hl7_numeric;
HParser *hl7_field_separator;
HParser *hl7_component_separator;
HParser *hl7_repetition_separator;
HParser *hl7_escape_character;
HParser *hl7_subcomponent_separator;

// Define parsers for specific fields
HParser *hl7_pid_3;
HParser *hl7_pid_5;

// Define parsers for segments
HParser *hl7_pid_segment;

// Define a parser for an HL7 message
HParser *hl7_message;

// Function to read file into memory
char *read_file(const char *filename, size_t *length) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        perror("Unable to open file");
        return NULL;
    }

    fseek(f, 0, SEEK_END);
    *length = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *data = malloc(*length);
    if (!data) {
        perror("Unable to allocate memory");
        fclose(f);
        return NULL;
    }

    if (fread(data, 1, *length, f) != *length) {
        perror("Unable to read file");
        free(data);
        fclose(f);
        return NULL;
    }

    fclose(f);
    return data;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    // Initialize parsers
    hl7_alphanumeric = h_token((const uint8_t *)"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", 62);
    hl7_numeric = h_token((const uint8_t *)"0123456789", 10);
    hl7_field_separator = h_ch('|');
    hl7_component_separator = h_ch('^');
    hl7_repetition_separator = h_ch('~');
    hl7_escape_character = h_ch('\\');
    hl7_subcomponent_separator = h_ch('&');

    hl7_pid_3 = h_sequence(hl7_alphanumeric, hl7_component_separator, hl7_alphanumeric, NULL);
    hl7_pid_5 = h_sequence(hl7_alphanumeric, hl7_component_separator, hl7_alphanumeric, hl7_component_separator, hl7_alphanumeric, NULL);

    hl7_pid_segment = h_sequence(
        h_token((const uint8_t *)"PID", 3),
        hl7_field_separator,
        hl7_numeric, // PID-1: Set ID - PID
        hl7_field_separator,
        hl7_pid_3, // PID-3: Patient Identifier List
        hl7_field_separator,
        hl7_pid_5, // PID-5: Patient Name
        NULL
    );

    hl7_message = h_sequence(
        hl7_pid_segment,
        NULL
    );

    size_t length;
    char *data = read_file(argv[1], &length);
    if (!data) {
        return 1;
    }

    HParseResult *result = h_parse(hl7_message, (const uint8_t *)data, length);
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 4);
    } else {
        printf("Parse failed.\n");
    }

    free(data);
    return 0;
}