#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_http_parser() {
    // Basic tokens
    HParser* sp = h_ch(' ');
    HParser* crlf = h_sequence(h_ch('\r'), h_ch('\n'), NULL);
    
    // HTTP version
    HParser* http_version = h_sequence(h_token("HTTP/"), 
                                     h_int_range(h_uint8(), 0, 9),
                                     h_ch('.'),
                                     h_int_range(h_uint8(), 0, 9),
                                     NULL);
    
    // Method
    HParser* method = h_choice(h_token("GET"),
                             h_token("POST"),
                             h_token("PUT"),
                             h_token("DELETE"),
                             h_token("HEAD"),
                             h_token("OPTIONS"),
                             h_token("TRACE"),
                             h_token("CONNECT"),
                             NULL);
    
    // URI - simplified for basic paths
    HParser* uri_char = h_choice(h_ch_range('a', 'z'),
                                h_ch_range('A', 'Z'),
                                h_ch_range('0', '9'),
                                h_ch('/'),
                                h_ch('_'),
                                h_ch('-'),
                                h_ch('.'),
                                NULL);
    HParser* uri = h_many1(uri_char);
    
    // Header field
    HParser* field_char = h_choice(h_ch_range('a', 'z'),
                                  h_ch_range('A', 'Z'),
                                  h_ch_range('0', '9'),
                                  h_ch('-'),
                                  NULL);
    HParser* field_name = h_many1(field_char);
    HParser* field_value = h_many1(h_choice(h_ch_range(32, 126), NULL));
    HParser* header_field = h_sequence(field_name,
                                     h_token(": "),
                                     field_value,
                                     crlf,
                                     NULL);
    
    // Headers section
    HParser* headers = h_many(header_field);
    
    // Request line
    HParser* request_line = h_sequence(method,
                                     sp,
                                     uri,
                                     sp,
                                     http_version,
                                     crlf,
                                     NULL);
    
    // Complete HTTP request
    return h_sequence(request_line,
                     headers,
                     crlf,
                     NULL);
}

int main() {
    HParser* parser = init_http_parser();
    
    const uint8_t input[] = 
        "GET /index.html HTTP/1.1\r\n"
        "Host: www.example.com\r\n"
        "User-Agent: Mozilla/5.0\r\n"
        "\r\n";
    
    HParseResult* result = h_parse(parser, input, strlen((char*)input));
    
    if(result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }
    
    return 0;
}