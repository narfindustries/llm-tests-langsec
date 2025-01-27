#include <hammer/hammer.h>
#include <stdio.h>

H_RULE(http_version, h_sequence(h_ch('H'), h_ch('T'), h_ch('T'), h_ch('P'), h_ch('/'), h_ch('1'), h_ch('.'), h_ch('1')));

H_RULE(header_name, h_token(h_ch_range('a', 'z')));
H_RULE(header_value, h_many(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch(' '))));
H_RULE(header, h_sequence(header_name, h_ch(':'), h_ch(' '), header_value, h_token(h_ch('\r')), h_ch('\n')));

H_RULE(method, h_choice(h_token(h_sequence(h_ch('G'), h_ch('E'), h_ch('T'))),
                       h_token(h_sequence(h_ch('P'), h_ch('O'), h_ch('S'), h_ch('T'))),
                       h_token(h_sequence(h_ch('H'), h_ch('E'), h_ch('A'), h_ch('D')))));

H_RULE(path, h_many1(h_choice(h_ch('/'), h_ch_range('a', 'z'), h_ch_range('0', '9'))));

H_RULE(request_line, h_sequence(method, h_ch(' '), path, h_ch(' '), http_version, h_token(h_ch('\r')), h_ch('\n')));

H_RULE(headers, h_many(header));

H_RULE(body, h_many(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch_range('0', '9'), h_ch(' '))));

H_RULE(http_request, h_sequence(request_line, headers, h_token(h_ch('\r')), h_ch('\n'), body));

void parse_http(const char* input) {
    HParser* parser = h_sequence(http_request, h_end_p());
    
    HParseResult* result = h_parse(parser, (const uint8_t*)input, strlen(input));
    
    if (result) {
        printf("Parsing succeeded\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }
}

int main() {
    const char* test_input = 
        "GET /path HTTP/1.1\r\n"
        "host: example.com\r\n"
        "content-type: text/plain\r\n"
        "\r\n"
        "Hello World";
        
    parse_http(test_input);
    return 0;
}