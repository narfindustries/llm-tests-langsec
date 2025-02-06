#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//Corrected helper function to parse HTTP-date (still simplified)
HParser http_date_parser() {
  return h_string("Date:"); //Example - needs a real date parser
}

//Corrected helper function to parse header fields
HParser header_field_parser(){
    return h_until(h_char(':'), h_not(h_char('\r')));
}

//Corrected helper function to parse header values
HParser header_value_parser(){
    return h_until(h_string("\r\n"), h_any_char());
}

//Corrected Helper function to parse a header field-value pair
HParser header_field_value_parser() {
    return h_seq(header_field_parser(),
                  h_char(':'),
                  h_whitespace(),
                  header_value_parser(),
                  h_map(h_tuple(h_first, h_second), h_pair));
}

//Corrected Parser for HTTP headers
HParser headers_parser() {
    return h_many(&header_field_value_parser());
}

//Corrected Simplified HTTP message parser (still incomplete)
HParser http_message_parser() {
    return h_seq(h_string("HTTP/1.1"),
                  h_whitespace(),
                  h_int(),
                  h_whitespace(),
                  h_string("OK"), //Simplified status line
                  h_whitespace(),
                  h_string("\r\n"),
                  headers_parser(),
                  h_string("\r\n"),
                  h_end());
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

    HParseResult result = h_parse(&http_message_parser(), buffer, fsize);

    if (result.status == H_PARSE_SUCCESS) {
        printf("HTTP message parsed successfully!\n");
        //Process the parsed result (result.value) - Add your logic here
    } else {
        fprintf(stderr, "HTTP message parsing failed at position %zu: %s\n", result.position, result.error);
    }

    free(buffer);
    return 0;
}
