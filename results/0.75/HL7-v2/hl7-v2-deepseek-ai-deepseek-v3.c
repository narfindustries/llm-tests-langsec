#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BUFFER_SIZE 1024

// Define parsers for basic HL7 data types
HParser *hl7_st_parser() {
    return h_many1(h_ch_range(32, 126)); // Printable ASCII characters
}

HParser *hl7_id_parser() {
    return h_ch_range(0, 127); // Any 7-bit ASCII character
}

HParser *hl7_dt_parser() {
    return h_repeat_n(h_ch_range('0', '9'), 8); // YYYYMMDD
}

HParser *hl7_tm_parser() {
    return h_repeat_n(h_ch_range('0', '9'), 6); // HHMMSS
}

HParser *hl7_dtm_parser() {
    return h_repeat_n(h_ch_range('0', '9'), 14); // YYYYMMDDHHMMSS
}

HParser *hl7_is_parser() {
    return h_ch_range(0, 127); // Any 7-bit ASCII character
}

HParser *hl7_ce_parser() {
    return h_sequence(hl7_st_parser(), h_ch('^'), hl7_st_parser(), h_ch('^'), hl7_st_parser(), NULL); // Code^Text^CodeSystem
}

HParser *hl7_cx_parser() {
    return h_sequence(hl7_st_parser(), h_ch('^'), hl7_st_parser(), h_ch('^'), hl7_st_parser(), NULL); // ID^CheckDigit^AssigningAuthority
}

HParser *hl7_xpn_parser() {
    return h_sequence(hl7_st_parser(), h_ch('^'), hl7_st_parser(), h_ch('^'), hl7_st_parser(), NULL); // FamilyName^GivenName^MiddleName
}

HParser *hl7_xad_parser() {
    return h_sequence(hl7_st_parser(), h_ch('^'), hl7_st_parser(), h_ch('^'), hl7_st_parser(), h_ch('^'), hl7_st_parser(), h_ch('^'), hl7_st_parser(), h_ch('^'), hl7_st_parser(), NULL); // Street^City^State^Zip^Country
}

// Define parsers for HL7 segments
HParser *hl7_msh_parser() {
    return h_sequence(
        h_ch('M'), h_ch('S'), h_ch('H'), h_ch('|'),
        hl7_st_parser(), h_ch('|'), // Field Separator
        hl7_st_parser(), h_ch('|'), // Encoding Characters
        hl7_st_parser(), h_ch('|'), // Sending Application
        hl7_st_parser(), h_ch('|'), // Sending Facility
        hl7_st_parser(), h_ch('|'), // Receiving Application
        hl7_st_parser(), h_ch('|'), // Receiving Facility
        hl7_dtm_parser(), h_ch('|'), // Date/Time of Message
        hl7_st_parser(), h_ch('|'), // Message Type
        hl7_st_parser(), h_ch('|'), // Message Control ID
        hl7_is_parser(), h_ch('|'), // Processing ID
        hl7_is_parser(), h_ch('|'), // Version ID
        NULL
    );
}

HParser *hl7_pid_parser() {
    return h_sequence(
        h_ch('P'), h_ch('I'), h_ch('D'), h_ch('|'),
        hl7_is_parser(), h_ch('|'), // Set ID
        hl7_cx_parser(), h_ch('|'), // Patient ID
        hl7_cx_parser(), h_ch('|'), // Patient Identifier List
        hl7_xpn_parser(), h_ch('|'), // Patient Name
        hl7_dt_parser(), h_ch('|'), // Date of Birth
        hl7_is_parser(), h_ch('|'), // Gender
        hl7_ce_parser(), h_ch('|'), // Race
        hl7_xad_parser(), h_ch('|'), // Address
        NULL
    );
}

// Main function
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    char buffer[MAX_BUFFER_SIZE];
    size_t bytes_read = fread(buffer, 1, MAX_BUFFER_SIZE, file);
    fclose(file);

    HParser *hl7_parser = hl7_msh_parser(); // Start with MSH parser
    HParseResult *result = h_parse(hl7_parser, (const uint8_t *)buffer, bytes_read);

    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    return 0;
}