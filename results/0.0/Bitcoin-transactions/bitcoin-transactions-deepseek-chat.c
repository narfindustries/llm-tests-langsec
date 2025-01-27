#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the Bitcoin transaction structure
typedef struct {
    uint32_t version;
    uint8_t input_count;
    uint8_t output_count;
    uint32_t lock_time;
} BitcoinTransaction;

// Define the Hammer parser for a Bitcoin transaction
HParser *bitcoin_transaction_parser() {
    return h_sequence(
        h_int32(),  // version
        h_uint8(),  // input_count
        h_uint8(),  // output_count
        h_int32(),  // lock_time
        NULL
    );
}

// Function to parse a Bitcoin transaction
BitcoinTransaction *parse_bitcoin_transaction(const uint8_t *data, size_t data_len) {
    HParser *parser = bitcoin_transaction_parser();
    HParseResult *result = h_parse(parser, data, data_len);

    if (!result) {
        fprintf(stderr, "Failed to parse Bitcoin transaction\n");
        return NULL;
    }

    BitcoinTransaction *tx = malloc(sizeof(BitcoinTransaction));
    tx->version = *(uint32_t *)result->ast->child[0]->token;
    tx->input_count = *(uint8_t *)result->ast->child[1]->token;
    tx->output_count = *(uint8_t *)result->ast->child[2]->token;
    tx->lock_time = *(uint32_t *)result->ast->child[3]->token;

    h_parse_result_free(result);
    return tx;
}

// Function to free a Bitcoin transaction
void free_bitcoin_transaction(BitcoinTransaction *tx) {
    if (tx) {
        free(tx);
    }
}

// Main function to test the parser
int main() {
    uint8_t data[] = {
        0x01, 0x00, 0x00, 0x00,  // version
        0x01,                    // input_count
        0x01,                    // output_count
        0x00, 0x00, 0x00, 0x00   // lock_time
    };
    size_t data_len = sizeof(data);

    BitcoinTransaction *tx = parse_bitcoin_transaction(data, data_len);
    if (tx) {
        printf("Version: %u\n", tx->version);
        printf("Input Count: %u\n", tx->input_count);
        printf("Output Count: %u\n", tx->output_count);
        printf("Lock Time: %u\n", tx->lock_time);
        free_bitcoin_transaction(tx);
    }

    return 0;
}