#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* create_msh_parser(void);
HParser* create_pid_parser(void);
HParser* create_pv1_parser(void);
HParser* create_obr_parser(void);
HParser* create_obx_parser(void);

// Field separators and delimiters
static const uint8_t FIELD_SEPARATOR = '|';
static const uint8_t COMPONENT_SEPARATOR = '^';
static const uint8_t REPEAT_SEPARATOR = '~';
static const uint8_t ESCAPE_CHAR = '\\';
static const uint8_t SUBCOMPONENT_SEPARATOR = '&';

// MSH segment parser
HParser* create_msh_parser(void) {
    HParser* field_sep = h_ch(FIELD_SEPARATOR);
    HParser* encoding_chars = h_sequence(h_ch(COMPONENT_SEPARATOR), 
                                      h_ch(REPEAT_SEPARATOR),
                                      h_ch(ESCAPE_CHAR),
                                      h_ch(SUBCOMPONENT_SEPARATOR), NULL);
    HParser* sending_app = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* sending_facility = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* receiving_app = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* receiving_facility = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* datetime = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* security = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* msg_type = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* msg_control_id = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* processing_id = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* version_id = h_many(h_not_in((uint8_t*)"|", 1));
    
    return h_sequence(h_token((uint8_t*)"MSH", 3),
                     field_sep,
                     encoding_chars,
                     field_sep, sending_app,
                     field_sep, sending_facility,
                     field_sep, receiving_app,
                     field_sep, receiving_facility,
                     field_sep, datetime,
                     field_sep, security,
                     field_sep, msg_type,
                     field_sep, msg_control_id,
                     field_sep, processing_id,
                     field_sep, version_id,
                     NULL);
}

// PID segment parser
HParser* create_pid_parser(void) {
    HParser* field = h_many(h_not_in((uint8_t*)"|", 1));
    HParser* field_sep = h_ch(FIELD_SEPARATOR);
    
    return h_sequence(h_token((uint8_t*)"PID", 3),
                     field_sep, field,  // Set ID
                     field_sep, field,  // Patient ID
                     field_sep, field,  // Patient ID List
                     field_sep, field,  // Alternate Patient ID
                     field_sep, field,  // Patient Name
                     field_sep, field,  // Mother's Maiden Name
                     field_sep, field,  // Date/Time of Birth
                     field_sep, field,  // Administrative Sex
                     NULL);
}

// Main HL7 message parser
HParser* create_hl7_parser(void) {
    HParser* segment = h_choice(create_msh_parser(),
                              create_pid_parser(),
                              NULL);
    return h_many(segment);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        fclose(fp);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(input, 1, size, fp) != size) {
        free(input);
        fclose(fp);
        fprintf(stderr, "Failed to read input file\n");
        return 1;
    }
    fclose(fp);

    HParser *parser = create_hl7_parser();
    HParseResult *result = h_parse(parser, input, size);

    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        return 1;
    }

    // TODO: Process parse result
    h_parse_result_free(result);
    free(input);
    return 0;
}