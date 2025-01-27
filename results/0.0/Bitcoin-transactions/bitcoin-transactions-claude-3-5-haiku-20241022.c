#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Bitcoin transaction structure
typedef struct {
    char* transaction_id;
    char* sender;
    char* recipient;
    double amount;
    uint64_t timestamp;
} BitcoinTransaction;

// Parser for Bitcoin transaction
static HParser* bitcoin_transaction_parser() {
    // Transaction ID: 64-character hexadecimal string
    HParser* transaction_id = h_repeat_n(h_choice(
        h_ch_range('0', '9'), 
        h_ch_range('a', 'f'), 
        h_ch_range('A', 'F')
    ), 64);

    // Alphanumeric sender/recipient addresses
    HParser* address = h_repeat_min_max(
        h_choice(
            h_ch_range('0', '9'), 
            h_ch_range('a', 'z'), 
            h_ch_range('A', 'Z')
        ), 
        26, 35
    );

    // Amount parser (decimal with up to 8 decimal places)
    HParser* amount = h_sequence(
        h_optional(h_ch('-')),
        h_repeat_min_max(h_ch_range('0', '9'), 1, 10),
        h_optional(h_sequence(
            h_ch('.'), 
            h_repeat_max(h_ch_range('0', '9'), 8)
        ))
    );

    // Timestamp parser (64-bit unsigned integer)
    HParser* timestamp = h_uint64();

    // Combine parsers into transaction structure
    return h_sequence(
        h_whitespace_before(transaction_id),
        h_whitespace_before(address),
        h_whitespace_before(address),
        h_whitespace_before(amount),
        h_whitespace_before(timestamp)
    );
}

// Semantic action to create Bitcoin transaction
static HAction bitcoin_transaction_action(const HParseResult* p, void* user_data) {
    if (!p || !p->ast) return NULL;

    BitcoinTransaction* txn = malloc(sizeof(BitcoinTransaction));
    
    // Extract and convert parsed fields
    txn->transaction_id = h_ast_to_string(p->ast->elements[0]);
    txn->sender = h_ast_to_string(p->ast->elements[1]);
    txn->recipient = h_ast_to_string(p->ast->elements[2]);
    
    // Convert amount to double
    char* amount_str = h_ast_to_string(p->ast->elements[3]);
    txn->amount = strtod(amount_str, NULL);
    free(amount_str);

    // Convert timestamp
    txn->timestamp = h_ast_to_uint64(p->ast->elements[4]);

    return txn;
}

// Validation function
int validate_bitcoin_transaction(BitcoinTransaction* txn) {
    if (!txn) return 0;
    
    // Basic validation checks
    if (strlen(txn->transaction_id) != 64) return 0;
    if (strlen(txn->sender) < 26 || strlen(txn->sender) > 35) return 0;
    if (strlen(txn->recipient) < 26 || strlen(txn->recipient) > 35) return 0;
    if (txn->amount <= 0) return 0;
    
    return 1;
}

// Main parsing function
BitcoinTransaction* parse_bitcoin_transaction(const char* input) {
    HParser* parser = bitcoin_transaction_parser();
    HParseResult* result = h_parse(parser, (const uint8_t*)input, strlen(input));
    
    if (!result || !result->ast) {
        h_parse_result_free(result);
        return NULL;
    }

    BitcoinTransaction* txn = bitcoin_transaction_action(result, NULL);
    h_parse_result_free(result);

    if (validate_bitcoin_transaction(txn)) {
        return txn;
    }

    free(txn);
    return NULL;
}

// Memory cleanup function
void free_bitcoin_transaction(BitcoinTransaction* txn) {
    if (txn) {
        free(txn->transaction_id);
        free(txn->sender);
        free(txn->recipient);
        free(txn);
    }
}

int main() {
    // Example usage
    const char* sample_txn = "a1b2c3d4e5f6g7h8 1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa 3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy 0.001 1231006505";
    
    BitcoinTransaction* parsed_txn = parse_bitcoin_transaction(sample_txn);
    
    if (parsed_txn) {
        printf("Transaction parsed successfully:\n");
        printf("ID: %s\n", parsed_txn->transaction_id);
        printf("Sender: %s\n", parsed_txn->sender);
        printf("Recipient: %s\n", parsed_txn->recipient);
        printf("Amount: %f\n", parsed_txn->amount);
        printf("Timestamp: %lu\n", parsed_txn->timestamp);
        
        free_bitcoin_transaction(parsed_txn);
    } else {
        printf("Failed to parse transaction\n");
    }

    return 0;
}