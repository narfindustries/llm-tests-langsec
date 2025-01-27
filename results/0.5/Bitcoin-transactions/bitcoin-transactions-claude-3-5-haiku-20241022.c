#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* transaction_id;
    char* sender;
    char* recipient;
    double amount;
    char* timestamp;
} BitcoinTransaction;

static HParser* bitcoin_transaction_parser(void) {
    HParser* transaction_id = h_token_pred(h_many1(h_choice(
        h_ch_range('0', '9'), 
        h_ch_range('a', 'f'), 
        h_ch_range('A', 'F')
    )), 64);

    HParser* address = h_token_pred(h_many1(h_choice(
        h_ch_range('0', '9'), 
        h_ch_range('a', 'z'), 
        h_ch_range('A', 'Z')
    )), 34);

    HParser* amount_parser = h_float_range(0, 21000000);

    HParser* timestamp_parser = h_token_pred(h_many1(h_digit()), 10);

    return h_sequence(
        transaction_id,   // Transaction ID
        h_whitespace(),
        address,          // Sender
        h_whitespace(), 
        address,          // Recipient
        h_whitespace(),
        amount_parser,    // Amount
        h_whitespace(),
        timestamp_parser, // Timestamp
        NULL
    );
}

static HParseResult* parse_bitcoin_transaction(HParser* parser, const char* input) {
    return h_parse(parser, (const uint8_t*)input, strlen(input));
}

BitcoinTransaction* create_bitcoin_transaction(HParseResult* result) {
    if (!result || !result->ast) return NULL;

    BitcoinTransaction* transaction = malloc(sizeof(BitcoinTransaction));
    
    transaction->transaction_id = strdup((char*)result->ast->elements[0]->data);
    transaction->sender = strdup((char*)result->ast->elements[2]->data);
    transaction->recipient = strdup((char*)result->ast->elements[4]->data);
    transaction->amount = *(double*)result->ast->elements[6]->data;
    transaction->timestamp = strdup((char*)result->ast->elements[8]->data);

    return transaction;
}

void free_bitcoin_transaction(BitcoinTransaction* transaction) {
    if (transaction) {
        free(transaction->transaction_id);
        free(transaction->sender);
        free(transaction->recipient);
        free(transaction->timestamp);
        free(transaction);
    }
}

int main() {
    HParser* parser = bitcoin_transaction_parser();
    const char* sample_input = "1a2b3c4d5e6f7g8h9i0j 1WbEWJUJUJUJUJUJUJUJUJUJUJUJU 1XyzAddressExample 10.5 1634567890";

    HParseResult* result = parse_bitcoin_transaction(parser, sample_input);
    
    if (result && result->ast) {
        BitcoinTransaction* transaction = create_bitcoin_transaction(result);
        
        printf("Transaction ID: %s\n", transaction->transaction_id);
        printf("Sender: %s\n", transaction->sender);
        printf("Recipient: %s\n", transaction->recipient);
        printf("Amount: %f\n", transaction->amount);
        printf("Timestamp: %s\n", transaction->timestamp);

        free_bitcoin_transaction(transaction);
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    h_parser_free(parser);
    return 0;
}