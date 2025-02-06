#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

hm_parser_t* parse_header_field(void) {
    return hm_regex("[a-zA-Z0-9-]+");
}

hm_parser_t* parse_header_value(void) {
    return hm_regex("[^\\r\\n]+");
}

hm_parser_t* parse_http_header(void) {
    return hm_seq(parse_header_field(), hm_string(": "), parse_header_value(), hm_string("\r\n"));
}

hm_parser_t* parse_http_headers(void) {
    return hm_many(parse_http_header());
}

hm_parser_t* parse_http_start_line(void) {
    return hm_seq(hm_regex("[A-Z]+"), hm_string(" "), hm_regex(".+"), hm_string(" HTTP/1.1\r\n"));
}

hm_parser_t* parse_http_message(void) {
    return hm_seq(parse_http_start_line(), parse_http_headers(), hm_string("\r\n")); 
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    char* buffer = (char*)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hm_parser_t* parser = parse_http_message();
    hm_result_t result = hm_parse(parser, buffer, fsize);

    if (result.success) {
        printf("HTTP message parsed successfully!\n");
    } else {
        fprintf(stderr, "HTTP message parsing failed at position %zu: %s\n", result.position, result.error);
    }

    hm_free(parser);
    free(buffer);
    return 0;
}

