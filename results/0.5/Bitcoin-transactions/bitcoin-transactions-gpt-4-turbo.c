#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Define the basic structures used in Bitcoin transactions
static HParser *uint256;
static HParser *varint;
static HParser *tx_in;
static HParser *tx_out;
static HParser *transaction;

// Util function to parse uint256 commonly used in Bitcoin for hashes
static HParsedToken *act_uint256(const HParseResult *p, void *user_data) {
    // Assuming uint256 is just a sequence of 32 bytes (little-endian)
    return H_MAKE_BYTES(p->ast->token);
}

// Parser for variable length integers used in Bitcoin
static HParser *build_varint() {
    return h_choice(h_sequence(h_bits(8, false), NULL),
                    h_sequence(h_bits(8, false), h_bits(16, false), NULL),
                    h_sequence(h_bits(8, false), h_bits(32, false), NULL),
                    h_sequence(h_bits(8, false), h_bits(64, false), NULL),
                    NULL);
}

// Parser for a transaction input
static HParser *build_tx_in() {
    return h_sequence(
        uint256, // previous output hash
        h_bits(32, false), // previous output index
        h_length_value(h_bits(16, false), h_uint8()), // script (length + data)
        h_bits(32, false), // sequence
        NULL);
}

// Parser for a transaction output
static HParser *build_tx_out() {
    return h_sequence(
        h_bits(64, false), // value in satoshis
        h_length_value(h_bits(16, false), h_uint8()), // script (length + data)
        NULL);
}

// Parser for a Bitcoin transaction
static HParser *build_transaction() {
    return h_sequence(
        h_bits(32, false), // version
        h_length_value(varint, tx_in), // inputs
        h_length_value(varint, tx_out), // outputs
        h_bits(32, false), // locktime
        NULL);
}

int main(int argc, char **argv) {
    HParser *uint256 = h_repeat_n(h_uint8(), 32);
    varint = build_varint();
    tx_in = build_tx_in();
    tx_out = build_tx_out();
    transaction = build_transaction();

    // Read binary data from stdin
    uint8_t *input = malloc(10000); // Adjust size as needed
    size_t input_size = fread(input, 1, 10000, stdin);

    // Parse the input data
    HParseResult *result = h_parse(transaction, input, input_size);
    if (result) {
        printf("Transaction parsed successfully!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "Failed to parse transaction\n");
    }

    // Cleanup
    h_parse_result_free(result);
    free(input);
    return 0;
}