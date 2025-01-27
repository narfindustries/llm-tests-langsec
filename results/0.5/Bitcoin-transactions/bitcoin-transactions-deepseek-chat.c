#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the Bitcoin transaction structure
typedef struct {
    uint32_t version;
    uint32_t input_count;
    uint32_t output_count;
    uint32_t lock_time;
} BitcoinTransaction;

// Define the Hammer parser for a Bitcoin transaction
HParser *bitcoin_transaction_parser() {
    return h_sequence(
        h_int32(),  // version
        h_int32(),  // input_count
        h_int32(),  // output_count
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
    tx->version = result->ast->seq->elements[0]->uint32;
    tx->input_count = result->ast->seq->elements[1]->uint32;
    tx->output_count = result->ast->seq->elements[2]->uint32;
    tx->lock_time = result->ast->seq->elements[3]->uint32;

    h_parse_result_free(result);
    return tx;
}

// Function to free a Bitcoin transaction
void free_bitcoin_transaction(BitcoinTransaction *tx) {
    if (tx) {
        free(tx);
    }
}

// Main function for testing
int main() {
    // Example Bitcoin transaction data
    uint8_t data[] = {
        0x01, 0x00, 0x00, 0x00,  // version
        0x01, 0x00, 0x00, 0x00,  // input_count
        0x01, 0x00, 0x00, 0x00,  // output_count
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