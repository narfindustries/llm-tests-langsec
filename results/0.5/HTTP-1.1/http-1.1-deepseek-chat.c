#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the HTTP/1.1 grammar using Hammer
HParser *http_grammar() {
    H_RULE(method, h_choice(
        h_token("GET"),
        h_token("POST"),
        h_token("PUT"),
        h_token("DELETE"),
        h_token("HEAD"),
        h_token("OPTIONS"),
        h_token("CONNECT"),
        h_token("TRACE"),
        h_token("PATCH"),
        NULL
    ));

    H_RULE(space, h_ch(' '));
    H_RULE(crlf, h_sequence(h_ch('\r'), h_ch('\n'), NULL));
    H_RULE(header_name, h_many1(h_alpha()));
    H_RULE(header_value, h_many1(h_choice(h_alpha(), h_ch('-'), h_ch('.'), h_ch('/'), h_ch(':'), h_ch(' '), NULL)));
    H_RULE(header, h_sequence(header_name, h_ch(':'), h_ch(' '), header_value, crlf, NULL));
    H_RULE(headers, h_many(header));
    H_RULE(request_line, h_sequence(method, space, h_many1(h_choice(h_alpha(), h_ch('/'), h_ch('.'), h_ch(':'), h_ch('-'), NULL)), space, h_token("HTTP/1.1"), crlf, NULL));
    H_RULE(http_request, h_sequence(request_line, headers, crlf, NULL));

    return http_request;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *input_file = fopen(argv[1], "r");
    if (!input_file) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(input_file, 0, SEEK_END);
    long input_size = ftell(input_file);
    fseek(input_file, 0, SEEK_SET);

    char *input_data = malloc(input_size + 1);
    if (!input_data) {
        perror("Failed to allocate memory for input data");
        fclose(input_file);
        return 1;
    }

    fread(input_data, 1, input_size, input_file);
    input_data[input_size] = '\0';
    fclose(input_file);

    HParser *parser = http_grammar();
    HParseResult *result = h_parse(parser, (const uint8_t *)input_data, input_size);

    if (result) {
        printf("Parsing succeeded!\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(input_data);
    return 0;
}