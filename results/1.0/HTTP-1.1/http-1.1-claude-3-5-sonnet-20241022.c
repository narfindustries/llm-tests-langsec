#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_http_parser() {
    // Basic token parsers
    HParser *sp = h_ch(' ');
    HParser *crlf = h_sequence(h_ch('\r'), h_ch('\n'), NULL);
    HParser *digit = h_ch_range('0', '9');
    HParser *alpha = h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), NULL);
    HParser *token_char = h_choice(
        alpha,
        digit,
        h_ch('!'), h_ch('#'), h_ch('$'), h_ch('%'), h_ch('&'), h_ch('\''),
        h_ch('*'), h_ch('+'), h_ch('-'), h_ch('.'), h_ch('^'), h_ch('_'),
        h_ch('`'), h_ch('|'), h_ch('~'), NULL
    );

    // Method
    HParser *method = h_choice(
        h_token("GET", 3),
        h_token("POST", 4),
        h_token("PUT", 3),
        h_token("DELETE", 6),
        h_token("HEAD", 4),
        h_token("OPTIONS", 7),
        h_token("TRACE", 5),
        h_token("CONNECT", 7),
        NULL
    );

    // Request-URI
    HParser *uri_char = h_choice(
        h_ch_range('a', 'z'),
        h_ch_range('A', 'Z'),
        h_ch_range('0', '9'),
        h_ch('-'), h_ch('.'), h_ch('_'), h_ch('~'),
        h_ch(':'), h_ch('/'), h_ch('?'), h_ch('#'),
        h_ch('['), h_ch(']'), h_ch('@'), h_ch('!'),
        h_ch('$'), h_ch('&'), h_ch('\''), h_ch('('),
        h_ch(')'), h_ch('*'), h_ch('+'), h_ch(','),
        h_ch(';'), h_ch('='), h_ch('%'),
        NULL
    );
    HParser *request_uri = h_many1(uri_char);

    // HTTP Version
    HParser *http_version = h_sequence(
        h_token("HTTP/", 5),
        digit,
        h_ch('.'),
        digit,
        NULL
    );

    // Header field components
    HParser *field_name = h_many1(token_char);
    HParser *field_content = h_many1(h_choice(
        h_ch_range(0x20, 0x7E),
        h_ch('\t'),
        NULL
    ));
    HParser *header_field = h_sequence(
        field_name,
        h_ch(':'),
        h_optional(sp),
        field_content,
        crlf,
        NULL
    );

    // Complete request line
    HParser *request_line = h_sequence(
        method,
        sp,
        request_uri,
        sp,
        http_version,
        crlf,
        NULL
    );

    // Complete HTTP message
    return h_sequence(
        request_line,
        h_many(header_field),
        crlf,
        NULL
    );
}

int main() {
    HParser *parser = init_http_parser();
    return 0;
}