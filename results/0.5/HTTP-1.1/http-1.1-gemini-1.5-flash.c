#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

//Helper function to parse a string until a newline
hm_parser_t* parse_line(void){
    return hm_string_until("\r\n");
}

hm_parser_t* parse_header_field(void){
    return hm_seq(parse_line(), hm_string("\r\n"), NULL);
}

hm_parser_t* parse_headers(void){
    return hm_many(parse_header_field);
}

hm_parser_t* parse_http_message(void){
    return hm_seq(parse_line(), parse_headers, NULL);
}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    unsigned char *buffer = (unsigned char *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hm_input_t input;
    input.data = buffer;
    input.len = fsize;

    hm_result_t result = hm_parse(parse_http_message(), &input);

    if (result.success) {
        printf("HTTP message parsed successfully!\n");
        // Process the parsed data here (result.value)
    } else {
        fprintf(stderr, "HTTP message parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}
