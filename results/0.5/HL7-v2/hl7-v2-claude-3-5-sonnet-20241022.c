#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char* field_separator;
    char* encoding_chars;
    char* sending_app;
    char* sending_facility;
    char* receiving_app;
    char* receiving_facility;
    char* datetime;
    char* message_type;
    char* message_control_id;
    char* processing_id;
    char* version_id;
} MSH_Segment;

typedef struct {
    char* id;
    char* patient_id;
    char* patient_name;
    char* dob;
    char* gender;
    char* address;
    char* phone;
} PID_Segment;

static HParser* make_field_parser() {
    return h_many1(h_not_in("|", 1));
}

static HParser* make_segment_parser() {
    HParser* field = make_field_parser();
    HParser* separator = h_ch('|');
    return h_sequence(field, separator, field, separator, field, NULL);
}

static HParser* make_hl7_parser() {
    return h_many1(make_segment_parser());
}

int main(int argc, char *argv[]) {
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
        fclose(file);
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        free(input);
        fclose(file);
        return 1;
    }

    HParser* parser = make_hl7_parser();
    HParseResult* result = h_parse(parser, input, size);

    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        fclose(file);
        return 1;
    }

    // TODO: Process parse result

    h_parse_result_free(result);
    free(input);
    fclose(file);
    return 0;
}