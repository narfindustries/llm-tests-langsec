#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// HL7 Field Parsers
HParser* parse_field_separator() {
    return h_ch('|');
}

HParser* parse_encoding_chars() {
    return h_sequence(h_ch('^'), h_ch('~'), h_ch('\\'), h_ch('&'), NULL);
}

HParser* parse_component() {
    return h_many(h_not_in("^~\\&|", 5));
}

HParser* parse_subcomponent() {
    return h_many(h_not_in("&|^~\\", 5));
}

// MSH Segment Parser
HParser* parse_msh() {
    return h_sequence(
        h_token((uint8_t*)"MSH", 3),
        parse_field_separator(),
        parse_encoding_chars(),
        parse_component(),  // Sending Application
        parse_field_separator(),
        parse_component(),  // Sending Facility
        parse_field_separator(),
        parse_component(),  // Receiving Application
        parse_field_separator(),
        parse_component(),  // Receiving Facility
        parse_field_separator(),
        parse_component(),  // DateTime
        parse_field_separator(),
        parse_component(),  // Security
        parse_field_separator(),
        parse_component(),  // Message Type
        parse_field_separator(),
        parse_component(),  // Message Control ID
        parse_field_separator(),
        parse_component(),  // Processing ID
        parse_field_separator(),
        parse_component(),  // Version ID
        NULL
    );
}

// PID Segment Parser
HParser* parse_pid() {
    return h_sequence(
        h_token((uint8_t*)"PID", 3),
        parse_field_separator(),
        parse_component(),  // Set ID
        parse_field_separator(),
        parse_component(),  // Patient ID
        parse_field_separator(),
        parse_component(),  // Patient ID List
        parse_field_separator(),
        parse_component(),  // Alternate Patient ID
        parse_field_separator(),
        parse_component(),  // Patient Name
        parse_field_separator(),
        parse_component(),  // Mother's Maiden Name
        parse_field_separator(),
        parse_component(),  // Date/Time of Birth
        parse_field_separator(),
        parse_component(),  // Sex
        NULL
    );
}

// OBR Segment Parser
HParser* parse_obr() {
    return h_sequence(
        h_token((uint8_t*)"OBR", 3),
        parse_field_separator(),
        parse_component(),  // Set ID
        parse_field_separator(),
        parse_component(),  // Placer Order Number
        parse_field_separator(),
        parse_component(),  // Filler Order Number
        parse_field_separator(),
        parse_component(),  // Universal Service ID
        parse_field_separator(),
        parse_component(),  // Priority
        parse_field_separator(),
        parse_component(),  // Requested DateTime
        NULL
    );
}

// Complete HL7 Message Parser
HParser* parse_hl7_message() {
    return h_sequence(
        parse_msh(),
        h_many(h_choice(parse_pid(), parse_obr(), NULL)),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        perror("Failed to read file");
        free(input);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = parse_hl7_message();
    HParseResult* result = h_parse(parser, input, size);

    if (result) {
        printf("Successfully parsed HL7 message\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse HL7 message\n");
    }

    free(input);
    return 0;
}