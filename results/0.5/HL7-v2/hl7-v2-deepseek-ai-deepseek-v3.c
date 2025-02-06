#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define parsers for basic HL7 data types
HParser *hl7_field_separator() {
    return h_ch('|');
}

HParser *hl7_encoding_chars() {
    return h_sequence(h_ch('^'), h_ch('~'), h_ch('\\'), h_ch('&'), NULL);
}

HParser *hl7_string() {
    return h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch(' '), h_ch('.'), h_ch(','), h_ch('-'), h_ch('_'), NULL));
}

HParser *hl7_date_time() {
    return h_sequence(h_many1(h_ch_range('0', '9')), h_optional(h_sequence(h_ch('.'), h_many1(h_ch_range('0', '9')), NULL)), NULL);
}

HParser *hl7_message_type() {
    return h_sequence(hl7_string(), h_ch('^'), hl7_string(), NULL);
}

HParser *hl7_processing_id() {
    return h_ch_range('P', 'T');
}

HParser *hl7_version_id() {
    return h_sequence(h_ch('2'), h_ch('.'), h_ch_range('0', '9'), NULL);
}

HParser *hl7_country_code() {
    return h_sequence(h_ch_range('A', 'Z'), h_ch_range('A', 'Z'), NULL);
}

HParser *hl7_character_set() {
    return h_choice(h_token((const uint8_t *)"ASCII", 5), h_token((const uint8_t *)"UNICODE", 7), h_token((const uint8_t *)"ISO-IR-6", 8), NULL);
}

HParser *hl7_language_code() {
    return h_sequence(h_ch_range('a', 'z'), h_ch_range('a', 'z'), NULL);
}

// Define parser for MSH segment
HParser *hl7_msh_segment() {
    return h_sequence(
        h_token((const uint8_t *)"MSH", 3),
        hl7_field_separator(),
        hl7_encoding_chars(),
        hl7_field_separator(),
        hl7_string(), // Sending Application
        hl7_field_separator(),
        hl7_string(), // Sending Facility
        hl7_field_separator(),
        hl7_string(), // Receiving Application
        hl7_field_separator(),
        hl7_string(), // Receiving Facility
        hl7_field_separator(),
        hl7_date_time(), // Date/Time of Message
        hl7_field_separator(),
        h_optional(hl7_string()), // Security
        hl7_field_separator(),
        hl7_message_type(), // Message Type
        hl7_field_separator(),
        hl7_string(), // Message Control ID
        hl7_field_separator(),
        hl7_processing_id(), // Processing ID
        hl7_field_separator(),
        hl7_version_id(), // Version ID
        hl7_field_separator(),
        h_optional(hl7_string()), // Sequence Number
        hl7_field_separator(),
        h_optional(hl7_string()), // Continuation Pointer
        hl7_field_separator(),
        h_optional(hl7_string()), // Accept Acknowledgment Type
        hl7_field_separator(),
        h_optional(hl7_string()), // Application Acknowledgment Type
        hl7_field_separator(),
        hl7_country_code(), // Country Code
        hl7_field_separator(),
        hl7_character_set(), // Character Set
        hl7_field_separator(),
        hl7_language_code(), // Principal Language
        hl7_field_separator(),
        h_optional(hl7_string()), // Alternate Character Set Handling
        NULL
    );
}

// Define parser for PID segment
HParser *hl7_pid_segment() {
    return h_sequence(
        h_token((const uint8_t *)"PID", 3),
        hl7_field_separator(),
        hl7_string(), // Set ID
        hl7_field_separator(),
        hl7_string(), // Patient ID
        hl7_field_separator(),
        hl7_string(), // Patient Identifier List
        hl7_field_separator(),
        h_optional(hl7_string()), // Alternate Patient ID
        hl7_field_separator(),
        hl7_string(), // Patient Name
        hl7_field_separator(),
        h_optional(hl7_string()), // Mother’s Maiden Name
        hl7_field_separator(),
        hl7_date_time(), // Date/Time of Birth
        hl7_field_separator(),
        hl7_string(), // Administrative Sex
        hl7_field_separator(),
        h_optional(hl7_string()), // Patient Alias
        hl7_field_separator(),
        hl7_string(), // Race
        hl7_field_separator(),
        hl7_string(), // Patient Address
        hl7_field_separator(),
        h_optional(hl7_string()), // County Code
        hl7_field_separator(),
        hl7_string(), // Phone Number
        hl7_field_separator(),
        h_optional(hl7_string()), // Phone Number (Business)
        hl7_field_separator(),
        hl7_language_code(), // Primary Language
        NULL
    );
}

// Main function
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

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *hl7_parser = h_sequence(hl7_msh_segment(), hl7_pid_segment(), NULL);
    HParseResult *result = h_parse(hl7_parser, data, file_size);

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}