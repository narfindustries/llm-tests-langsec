#include <hammer/hammer.h>
#include <hammer/glue.h>

// Primitive parsers
static HParser *whitespace;
static HParser *crlf;
static HParser *digit;
static HParser *digits;
static HParser *token;
static HParser *quoted_string;
static HParser *header_value;

// Core rules
static HParser *request_line;
static HParser *status_line;
static HParser *header;
static HParser *message_body;
static HParser *http_message;

// Helper function to build parsers
void init_parsers() {
    whitespace = h_many(h_ch(' '));
    crlf = h_token("\r\n", 2);
    digit = h_ch_range('0', '9');
    digits = h_many1(digit);
    token = h_many1(h_butnot(h_ch_range(0, 31), h_ch(' ')));
    quoted_string = h_sequence(h_ch('"'), h_kleene(h_ch_range(32, 127)), h_ch('"'), NULL);

    header_value = h_sequence(
        whitespace,
        h_choice(
            quoted_string,
            token,
            NULL
        ),
        whitespace,
        NULL
    );

    request_line = h_sequence(
        token, // Method
        h_many1(h_ch(' ')),
        token, // Request-URI
        h_many1(h_ch(' ')),
        token, // HTTP-Version
        crlf,
        NULL
    );

    status_line = h_sequence(
        token, // HTTP-Version
        h_many1(h_ch(' ')),
        digits, // Status-Code
        h_many1(h_ch(' ')),
        token, // Reason-Phrase
        crlf,
        NULL
    );

    header = h_sequence(
        token, // field-name
        h_token(": ", 2),
        header_value, // field-value
        crlf,
        NULL
    );

    message_body = h_greedy(h_any(), NULL);

    http_message = h_sequence(
        h_choice(request_line, status_line, NULL),
        h_many(header),
        crlf,
        message_body,
        NULL
    );
}

int main(int argc, char *argv[]) {
    init_parsers();
    HParseResult *result = h_parse(http_message, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 4);
    } else {
        printf("Parse failed!\n");
    }
    return 0;
}