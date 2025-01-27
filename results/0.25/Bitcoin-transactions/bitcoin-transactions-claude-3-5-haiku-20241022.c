#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define Bitcoin transaction structure
typedef struct {
    char* transaction_id;
    char* sender;
    char* recipient;
    double amount;
    uint64_t timestamp;
} BitcoinTransaction;

// Parser for Bitcoin transaction
static HParser* bitcoin_transaction_parser() {
    // Transaction ID: hexadecimal string of 64 characters
    HParser* transaction_id = h_repeat_n(h_choice(
        h_ch_range('0', '9'), 
        h_ch_range('a', 'f'), 
        h_ch_range('A', 'F')
    ), 64);

    // Wallet address: alphanumeric string
    HParser* wallet_address = h_repeat_n(h_choice(
        h_ch_range('0', '9'), 
        h_ch_range('a', 'z'), 
        h_ch_range('A', 'Z')
    ), 34);

    // Amount: decimal number
    HParser* amount = h_float_range(0, INFINITY);

    // Timestamp: 64-bit unsigned integer
    HParser* timestamp = h_uint64();

    // Combine parsers into transaction structure
    return h_struct(
        h_field_str("transaction_id", transaction_id),
        h_field_str("sender", wallet_address),
        h_field_str("recipient", wallet_address),
        h_field_float("amount", amount),
        h_field_uint64("timestamp", timestamp)
    );
}

// Validation function
int validate_bitcoin_transaction(BitcoinTransaction* tx) {
    if (!tx) return 0;
    if (!tx->transaction_id || strlen(tx->transaction_id) != 64) return 0;
    if (!tx->sender || strlen(tx->sender) != 34) return 0;
    if (!tx->recipient || strlen(tx->recipient) != 34) return 0;
    if (tx->amount <= 0) return 0;
    return 1;
}

// Main parsing and validation function
int parse_bitcoin_transactions(const uint8_t* input, size_t len) {
    HParser* parser = bitcoin_transaction_parser();
    HParseResult* result = h_parse(parser, input, len);

    if (!result || !result->ast) {
        return 0;
    }

    BitcoinTransaction tx = {
        .transaction_id = h_ast_get_str(result->ast, "transaction_id"),
        .sender = h_ast_get_str(result->ast, "sender"),
        .recipient = h_ast_get_str(result->ast, "recipient"),
        .amount = h_ast_get_float(result->ast, "amount"),
        .timestamp = h_ast_get_uint64(result->ast, "timestamp")
    };

    int is_valid = validate_bitcoin_transaction(&tx);
    h_parse_result_free(result);
    return is_valid;
}

int main() {
    // Example usage
    const char* sample_tx = "1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
                             "wallet1234567890abcdefghijklmnopqrst"
                             "wallet9876543210abcdefghijklmnopqrst"
                             "0.01"
                             "1634567890";

    int result = parse_bitcoin_transactions((const uint8_t*)sample_tx, strlen(sample_tx));
    printf("Transaction validation result: %s\n", result ? "Valid" : "Invalid");

    return 0;
}