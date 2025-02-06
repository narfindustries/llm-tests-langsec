#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

HParser *bitcoin_version() {
    return h_int32();
}

HParser *bitcoin_varint() {
    return h_choice(h_bits(8, false), h_bits(16, false), h_bits(32, false), NULL);
}

HParser *bitcoin_input() {
    return h_sequence(
        h_bits(256, false), // Previous Transaction Hash
        h_bits(32, false),  // Previous Output Index
        bitcoin_varint(),   // Script Length
        h_many(bitcoin_varint()), // ScriptSig
        h_bits(32, false),  // Sequence
        NULL
    );
}

HParser *bitcoin_output() {
    return h_sequence(
        h_bits(64, false),  // Value
        bitcoin_varint(),   // Script Length
        h_many(bitcoin_varint()), // ScriptPubKey
        NULL
    );
}

HParser *bitcoin_transaction() {
    return h_sequence(
        bitcoin_version(),  // Version
        bitcoin_varint(),   // Input Count
        h_many(bitcoin_input()), // Inputs
        bitcoin_varint(),   // Output Count
        h_many(bitcoin_output()), // Outputs
        h_bits(32, false),  // Lock Time
        h_optional(h_sequence(
            h_bits(8, false), // Marker
            h_bits(8, false), // Flag
            bitcoin_varint(), // Witness Count
            h_many(h_many(bitcoin_varint())), // Witness Data
            NULL
        )),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = bitcoin_transaction();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing succeeded.\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(buffer);
    return 0;
}