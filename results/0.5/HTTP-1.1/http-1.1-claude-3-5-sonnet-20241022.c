#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_http_parser(void) {
    // Basic tokens
    HParser* sp = h_ch(' ');
    HParser* crlf = h_sequence(h_ch('\r'), h_ch('\n'), NULL);
    HParser* digit = h_ch_range('0', '9');
    HParser* alpha = h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), NULL);
    HParser* token_char = h_choice(alpha, digit, 
        h_ch('!'), h_ch('#'), h_ch('$'), h_ch('%'), h_ch('&'), h_ch('\''),
        h_ch('*'), h_ch('+'), h_ch('-'), h_ch('.'), h_ch('^'), h_ch('_'),
        h_ch('`'), h_ch('|'), h_ch('~'), NULL);

    // HTTP version
    HParser* http_version = h_sequence(h_token("HTTP/"), 
        digit, h_ch('.'), digit, NULL);

    // Request target
    HParser* request_target = h_many1(h_choice(token_char, h_ch('/'), h_ch('?'), h_ch('='), NULL));

    // Method
    HParser* method = h_choice(
        h_token("GET"),
        h_token("POST"),
        h_token("PUT"),
        h_token("DELETE"),
        h_token("HEAD"),
        h_token("OPTIONS"),
        h_token("TRACE"),
        h_token("CONNECT"),
        NULL);

    // Header field components
    HParser* field_name = h_many1(token_char);
    HParser* field_value = h_many(h_choice(h_ch_range(0x20, 0x7E), sp, NULL));
    HParser* header_field = h_sequence(field_name, h_ch(':'), h_optional(sp),
        field_value, crlf, NULL);

    // Request line
    HParser* request_line = h_sequence(method, sp, request_target, sp,
        http_version, crlf, NULL);

    // Complete HTTP request
    return h_sequence(request_line,
        h_many(header_field),
        crlf,
        NULL);
}

int main(int argc, char** argv) {
    HParser* parser = init_http_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }

    return 0;
}