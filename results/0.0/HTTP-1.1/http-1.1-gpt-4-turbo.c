#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define basic parsers for HTTP components
static HParser *whitespace = h_many(h_ch(' '));
static HParser *newline = h_ch('\n');
static HParser *cr = h_ch('\r');
static HParser *crlf = h_sequence(cr, newline, NULL);
static HParser *digit = h_ch_range('0', '9');
static HParser *digits = h_many1(digit);

// Token parsers
static HParser *token_char = h_butnot(h_any(), h_ch(' '));
static HParser *token = h_many1(token_char);

// Define parsers for HTTP headers
static HParser *header_name = h_many1(h_butnot(h_any(), h_ch(':')));
static HParser *header_value = h_many1(h_butnot(h_any(), crlf));
static HParser *header = h_sequence(header_name, h_ch(':'), whitespace, header_value, crlf, NULL);

// Define parser for HTTP request line
static HParser *method = token;
static HParser *uri = token;
static HParser *http_version = h_sequence(h_string("HTTP/"), digits, h_ch('.'), digits, NULL);
static HParser *request_line = h_sequence(method, whitespace, uri, whitespace, http_version, crlf, NULL);

// Define parser for HTTP request
static HParser *headers = h_many(header);
static HParser *message_body = h_many(h_any());
static HParser *http_request = h_sequence(request_line, headers, crlf, message_body, NULL);

// Main function to parse HTTP requests
int main(int argc, char *argv[]) {
    HParser *parser = http_request;
    HParseResult *result = h_parse(parser, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed.\n");
    }
    h_parse_result_free(result);
    return 0;
}