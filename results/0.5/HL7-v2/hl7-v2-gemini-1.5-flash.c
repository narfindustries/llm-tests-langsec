#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

hm_parser_t* parse_hl7_field(void) {
    return hm_string("test");
}

hm_parser_t* parse_hl7_segment(void) {
    hm_parser_t* parser = hm_seq(
        hm_string("PID"),
        hm_many(parse_hl7_field()),
        hm_string("\r")
    );
    return parser;
}

hm_parser_t* parse_hl7_message(void) {
    hm_parser_t* parser = hm_many(parse_hl7_segment());
    return parser;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* buffer = (char*)malloc(fsize + 1);
    fread(buffer, 1, fsize, fp);
    buffer[fsize] = '\0';
    fclose(fp);

    hm_parser_t* message_parser = parse_hl7_message();
    hm_result_t result = hm_parse(message_parser, buffer);

    if (result.success) {
        printf("HL7 message parsed successfully!\n");
    } else {
        fprintf(stderr, "HL7 parsing failed at position %zu: %s\n", result.position, result.error);
    }

    hm_free(message_parser);
    free(buffer);
    return 0;
}
