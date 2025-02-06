#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//Helper functions to simplify parser creation
static hm_parser_t* make_string_parser(const char* str){
    return hm_string(str);
}

static hm_parser_t* make_whitespace_parser(){
    return hm_regex("[ \\t]+");
}

static hm_parser_t* make_crlf_parser(){
    return hm_string("\r\n");
}

static hm_parser_t* make_header_field_parser(const char* fieldName){
    return hm_seq(hm_string(fieldName), make_whitespace_parser(), hm_char(':'), make_whitespace_parser(), hm_until(make_crlf_parser()));
}

static hm_parser_t* make_header_parser(){
    hm_parser_t* header_field = hm_choice(
            make_header_field_parser("Cache-Control"),
            make_header_field_parser("Connection"),
            make_header_field_parser("Date"),
            make_header_field_parser("Expect"),
            make_header_field_parser("From"),
            make_header_field_parser("Host"),
            make_header_field_parser("If-Match"),
            make_header_field_parser("If-Modified-Since"),
            make_header_field_parser("If-None-Match"),
            make_header_field_parser("If-Range"),
            make_header_field_parser("If-Unmodified-Since"),
            make_header_field_parser("Max-Forwards"),
            make_header_field_parser("Pragma"),
            make_header_field_parser("Proxy-Authorization"),
            make_header_field_parser("Range"),
            make_header_field_parser("Referer"),
            make_header_field_parser("TE"),
            make_header_field_parser("User-Agent"),
            make_header_field_parser("Upgrade"),
            make_header_field_parser("Via"),
            make_header_field_parser("Warning"),
            make_header_field_parser("Authorization"),
            make_header_field_parser("Accept-Ranges"),
            make_header_field_parser("Age"),
            make_header_field_parser("Content-Encoding"),
            make_header_field_parser("Content-Language"),
            make_header_field_parser("Content-Length"),
            make_header_field_parser("Content-Location"),
            make_header_field_parser("Content-MD5"),
            make_header_field_parser("Content-Range"),
            make_header_field_parser("Content-Type"),
            make_header_field_parser("ETag"),
            make_header_field_parser("Expires"),
            make_header_field_parser("Last-Modified"),
            make_header_field_parser("Location"),
            make_header_field_parser("Proxy-Authenticate"),
            make_header_field_parser("Retry-After"),
            make_header_field_parser("Server"),
            make_header_field_parser("Set-Cookie"),
            make_header_field_parser("Trailer"),
            make_header_field_parser("Transfer-Encoding"),
            make_header_field_parser("Vary"),
            make_header_field_parser("WWW-Authenticate"),
            make_header_field_parser("Allow"),
            make_header_field_parser("Content-Disposition")
    );
    return hm_many(header_field);
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

    char *buffer = malloc(fsize + 1);
    fread(buffer, 1, fsize, fp);
    buffer[fsize] = '\0'; //Null terminate for safety
    fclose(fp);

    hm_parser_t* http_parser = make_header_parser();

    hm_result_t result = hm_parse(http_parser, buffer);

    if (result.success) {
        printf("HTTP message parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse HTTP message: %s\n", result.error);
    }

    hm_free(http_parser);
    free(buffer);
    return 0;
}
