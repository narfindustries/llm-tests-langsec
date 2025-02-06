#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static HParser* parse_string() {
    return h_many1(h_ch_range('A', 'Z'));
}

static HParser* parse_numeric() {
    return h_many1(h_ch_range('0', '9'));
}

static HParser* parse_datetime() {
    return h_sequence(
        h_many(h_ch_range('0', '9')),
        h_optional(h_ch('+')),
        h_optional(h_ch('-')),
        NULL
    );
}

static HParser* parse_msh_segment() {
    return h_sequence(
        h_literal("MSH"),
        h_many(h_not(h_ch('|'))),
        NULL
    );
}

static HParser* parse_pid_segment() {
    return h_sequence(
        h_literal("PID"),
        h_many(h_not(h_ch('|'))),
        NULL
    );
}

static HParser* parse_obr_segment() {
    return h_sequence(
        h_literal("OBR"),
        h_many(h_not(h_ch('|'))),
        NULL
    );
}

static HParser* parse_hl7_message() {
    return h_sequence(
        parse_msh_segment(),
        parse_pid_segment(),
        h_optional(parse_obr_segment()),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("File open error");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = parse_hl7_message();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        printf("HL7 v2 message parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "HL7 v2 message parsing failed\n");
    }

    free(buffer);
    h_parser_free(parser);
    return 0;
}