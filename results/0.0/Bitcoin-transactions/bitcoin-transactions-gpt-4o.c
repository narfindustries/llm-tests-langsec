#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *varint_parser() {
    return h_choice(
        h_uint8(),
        h_sequence(h_uint8(), h_uint16(), NULL),
        h_sequence(h_uint8(), h_uint32(), NULL),
        h_sequence(h_uint8(), h_uint64(), NULL),
        NULL
    );
}

HParser *script_parser() {
    return h_sequence(varint_parser(), h_many(h_uint8()), NULL);
}

HParser *input_parser() {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),  // Previous Transaction Hash
        h_uint32(),                // Output Index
        script_parser(),           // Signature Script
        h_uint32(),                // Sequence
        NULL
    );
}

HParser *output_parser() {
    return h_sequence(
        h_uint64(),                // Value
        script_parser(),           // Public Key Script
        NULL
    );
}

HParser *transaction_parser() {
    return h_sequence(
        h_uint32(),                // Version
        varint_parser(),           // Input Count
        h_many(input_parser()),    // Inputs
        varint_parser(),           // Output Count
        h_many(output_parser()),   // Outputs
        h_uint32(),                // Lock Time
        NULL
    );
}

void parse_transaction_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = transaction_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Transaction parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse transaction.\n");
    }

    free(buffer);
    h_parser_free(parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <transaction_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_transaction_file(argv[1]);
    return EXIT_SUCCESS;
}