#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Define the structure of a Bitcoin transaction input
static HParser *bitcoin_input() {
    return h_sequence(
        h_bytes(32), // Transaction Hash
        h_uint32(),  // Output Index
        h_len_value(h_uint16(), h_bytes), // Script (with length prefix)
        h_uint32(),  // Sequence
        NULL
    );
}

// Define the structure of a Bitcoin transaction output
static HParser *bitcoin_output() {
    return h_sequence(
        h_uint64(),  // Value in Satoshis
        h_len_value(h_uint16(), h_bytes), // Script (with length prefix)
        NULL
    );
}

// Define the structure of a Bitcoin transaction
static HParser *bitcoin_transaction() {
    return h_sequence(
        h_uint32(),  // Version
        h_len_value(h_varint(), bitcoin_input), // Inputs
        h_len_value(h_varint(), bitcoin_output), // Outputs
        h_uint32(),  // Locktime
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *btc_parser = bitcoin_transaction();
    uint8_t *input_data;
    size_t input_size;
    HParseResult *result;

    // Read input data from stdin or a file
    if (argc > 1) {
        FILE *f = fopen(argv[1], "rb");
        if (!f) {
            fprintf(stderr, "Failed to open file %s\n", argv[1]);
            return 1;
        }
        fseek(f, 0, SEEK_END);
        input_size = ftell(f);
        fseek(f, 0, SEEK_SET);
        input_data = malloc(input_size);
        fread(input_data, 1, input_size, f);
        fclose(f);
    } else {
        fprintf(stderr, "Usage: %s <bitcoin_transaction_file>\n", argv[0]);
        return 1;
    }

    // Parse the input data
    result = h_parse(btc_parser, input_data, input_size);
    if (result) {
        printf("Bitcoin transaction parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse Bitcoin transaction.\n");
    }

    free(input_data);
    h_parser_free(btc_parser);
    return 0;
}