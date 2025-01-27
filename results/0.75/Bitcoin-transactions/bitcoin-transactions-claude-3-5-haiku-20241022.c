#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Bitcoin transaction parser using Hammer parsing library

typedef struct {
    char* txid;
    uint64_t amount;
    char* sender;
    char* recipient;
    uint64_t timestamp;
} BitcoinTransaction;

static HParser* bitcoin_transaction_parser() {
    // Transaction ID: 64 hex characters
    HParser* txid = h_repeat_n(h_choice(
        h_ch_range('0', '9'),
        h_ch_range('a', 'f'),
        h_ch_range('A', 'F')
    ), 64);

    // Amount: positive integer
    HParser* amount = h_uint64();

    // Sender and recipient: alphanumeric strings
    HParser* address = h_many1(h_choice(
        h_ch_range('0', '9'),
        h_ch_range('a', 'z'),
        h_ch_range('A', 'Z')
    ));

    // Timestamp: Unix epoch time
    HParser* timestamp = h_uint64();

    // Full transaction structure
    HParser* transaction = h_sequence(
        h_whitespace_before(txid),
        h_whitespace_before(amount),
        h_whitespace_before(address),  // sender
        h_whitespace_before(address),  // recipient
        h_whitespace_before(timestamp),
        NULL
    );

    return transaction;
}

int main(int argc, char** argv) {
    // Initialize Hammer parser
    HParserBackend backend = HParserBackend_Packrat;
    h_init(&backend);

    // Create transaction parser
    HParser* parser = bitcoin_transaction_parser();

    // Example transaction input
    const char* input = "abcd1234... 1000000 sender_addr recipient_addr 1634567890";
    
    // Parse transaction
    HParseResult* result = h_parse(parser, (const uint8_t*)input, strlen(input));

    if (result && result->ast) {
        // Extract and print transaction details
        BitcoinTransaction tx = {0};
        // TODO: Implement AST traversal and transaction extraction
        
        printf("Transaction parsed successfully\n");
    } else {
        printf("Transaction parsing failed\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_destroy(parser);

    return 0;
}