#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function prototypes for initializing parsers
HParser *init_hl7_alphanumeric();
HParser *init_hl7_numeric();
HParser *init_hl7_field_separator();
HParser *init_hl7_component_separator();
HParser *init_hl7_repetition_separator();
HParser *init_hl7_escape_character();
HParser *init_hl7_subcomponent_separator();
HParser *init_hl7_msh_field();
HParser *init_hl7_pid_field();
HParser *init_hl7_pv1_field();
HParser *init_hl7_obr_field();
HParser *init_hl7_obx_field();
HParser *init_hl7_msh_segment();
HParser *init_hl7_pid_segment();
HParser *init_hl7_pv1_segment();
HParser *init_hl7_obr_segment();
HParser *init_hl7_obx_segment();
HParser *init_hl7_message();

// Main parsers
HParser *hl7_alphanumeric;
HParser *hl7_numeric;
HParser *hl7_field_separator;
HParser *hl7_component_separator;
HParser *hl7_repetition_separator;
HParser *hl7_escape_character;
HParser *hl7_subcomponent_separator;
HParser *hl7_msh_field;
HParser *hl7_pid_field;
HParser *hl7_pv1_field;
HParser *hl7_obr_field;
HParser *hl7_obx_field;
HParser *hl7_msh_segment;
HParser *hl7_pid_segment;
HParser *hl7_pv1_segment;
HParser *hl7_obr_segment;
HParser *hl7_obx_segment;
HParser *hl7_message;

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    // Initialize parsers
    hl7_alphanumeric = init_hl7_alphanumeric();
    hl7_numeric = init_hl7_numeric();
    hl7_field_separator = init_hl7_field_separator();
    hl7_component_separator = init_hl7_component_separator();
    hl7_repetition_separator = init_hl7_repetition_separator();
    hl7_escape_character = init_hl7_escape_character();
    hl7_subcomponent_separator = init_hl7_subcomponent_separator();
    hl7_msh_field = init_hl7_msh_field();
    hl7_pid_field = init_hl7_pid_field();
    hl7_pv1_field = init_hl7_pv1_field();
    hl7_obr_field = init_hl7_obr_field();
    hl7_obx_field = init_hl7_obx_field();
    hl7_msh_segment = init_hl7_msh_segment();
    hl7_pid_segment = init_hl7_pid_segment();
    hl7_pv1_segment = init_hl7_pv1_segment();
    hl7_obr_segment = init_hl7_obr_segment();
    hl7_obx_segment = init_hl7_obx_segment();
    hl7_message = init_hl7_message();

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *data = malloc(length);
    if (data) {
        fread(data, 1, length, file);
        HParseResult *result = h_parse(hl7_message, data, length);
        if (result) {
            printf("HL7 message parsed successfully.\n");
        } else {
            fprintf(stderr, "Failed to parse HL7 message.\n");
        }
        free(data);
    } else {
        fprintf(stderr, "Failed to allocate memory for file data.\n");
    }
    fclose(file);
    return 0;
}

// Initialization functions for parsers
HParser *init_hl7_alphanumeric() {
    return h_token((const uint8_t *)"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789", 62);
}

HParser *init_hl7_numeric() {
    return h_token((const uint8_t *)"0123456789", 10);
}

HParser *init_hl7_field_separator() {
    return h_ch('|');
}

HParser *init_hl7_component_separator() {
    return h_ch('^');
}

HParser *init_hl7_repetition_separator() {
    return h_ch('~');
}

HParser *init_hl7_escape_character() {
    return h_ch('\\');
}

HParser *init_hl7_subcomponent_separator() {
    return h_ch('&');
}

HParser *init_hl7_msh_field() {
    return h_sequence(hl7_alphanumeric, NULL);
}

HParser *init_hl7_pid_field() {
    return h_sequence(hl7_alphanumeric, NULL);
}

HParser *init_hl7_pv1_field() {
    return h_sequence(hl7_alphanumeric, NULL);
}

HParser *init_hl7_obr_field() {
    return h_sequence(hl7_alphanumeric, NULL);
}

HParser *init_hl7_obx_field() {
    return h_sequence(hl7_alphanumeric, NULL);
}

HParser *init_hl7_msh_segment() {
    return h_sequence(hl7_msh_field, hl7_field_separator, NULL);
}

HParser *init_hl7_pid_segment() {
    return h_sequence(hl7_pid_field, hl7_field_separator, NULL);
}

HParser *init_hl7_pv1_segment() {
    return h_sequence(hl7_pv1_field, hl7_field_separator, NULL);
}

HParser *init_hl7_obr_segment() {
    return h_sequence(hl7_obr_field, hl7_field_separator, NULL);
}

HParser *init_hl7_obx_segment() {
    return h_sequence(hl7_obx_field, hl7_field_separator, NULL);
}

HParser *init_hl7_message() {
    return h_sequence(
        hl7_msh_segment,
        hl7_pid_segment,
        hl7_pv1_segment,
        hl7_obr_segment,
        hl7_obx_segment,
        NULL
    );
}