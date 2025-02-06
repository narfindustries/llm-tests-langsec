#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define VAR_INT_MAX_SIZE 9

typedef struct {
    uint32_t version;
    uint64_t tx_in_count;
    struct tx_in {
        uint8_t txid[32];
        uint32_t vout;
        uint8_t* scriptSig;
        uint32_t sequence;
    }* tx_in;
    uint64_t tx_out_count;
    struct tx_out {
        uint64_t value;
        uint8_t* scriptPubKey;
    }* tx_out;
    uint32_t lock_time;
} transaction_t;

typedef struct {
    uint32_t version;
    uint8_t prev_block[32];
    uint8_t merkle_root[32];
    uint32_t timestamp;
    uint32_t target;
    uint32_t nonce;
    uint64_t transaction_count;
    transaction_t* transactions;
} block_t;

HParser* var_int_parser() {
    HParser* parser = h_choice(
        h_peek(h_uint8, 0xfd),
        h_peek(h_uint8, 0xfe),
        h_peek(h_uint8, 0xff)
    );

    HParser* parsers[] = {
        h_bind(h_uint8, h_uint16),
        h_bind(h_uint16, h_uint32),
        h_bind(h_uint32, h_uint64)
    };

    return h_bind(parser, h_choice(parsers[0], parsers[1], parsers[2]));
}

HParser* transaction_parser() {
    return h_bind(
        h_sequence(
            h_uint32,
            var_int_parser(),
            h_array_of(
                h_bind(
                    h_sequence(
                        h_array_of(h_uint8, 32),
                        h_uint32,
                        h_array_of(h_uint8),
                        h_uint32
                    ),
                    h_struct(sizeof(transaction_t))
                ),
                var_int_parser()
            ),
            var_int_parser(),
            h_array_of(
                h_bind(
                    h_sequence(
                        h_uint64,
                        h_array_of(h_uint8)
                    ),
                    h_struct(sizeof(transaction_t))
                ),
                var_int_parser()
            ),
            h_uint32
        ),
        h_struct(sizeof(block_t))
    );
}

HParser* block_parser() {
    return h_bind(
        h_sequence(
            h_uint32,
            h_array_of(h_uint8, 32),
            h_array_of(h_uint8, 32),
            h_uint32,
            h_uint32,
            h_uint32,
            var_int_parser(),
            h_array_of(transaction_parser(), var_int_parser())
        ),
        h_struct(sizeof(block_t))
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    HParser* parser = block_parser();
    HParseResult* result = h_parse(parser, data, file_size);

    if (result->status != H_OK) {
        printf("Error parsing file: %s\n", result->error);
        return 1;
    }

    block_t* block = result->value;
    printf("Block version: %u\n", block->version);
    printf("Previous block hash: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", block->prev_block[i]);
    }
    printf("\n");
    printf("Merkle root: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", block->merkle_root[i]);
    }
    printf("\n");
    printf("Timestamp: %u\n", block->timestamp);
    printf("Target: %u\n", block->target);
    printf("Nonce: %u\n", block->nonce);
    printf("Transaction count: %llu\n", block->transaction_count);

    for (int i = 0; i < block->transaction_count; i++) {
        transaction_t* transaction = block->transactions[i];
        printf("Transaction version: %u\n", transaction->version);
        printf("Transaction input count: %llu\n", transaction->tx_in_count);
        printf("Transaction output count: %llu\n", transaction->tx_out_count);
        printf("Lock time: %u\n", transaction->lock_time);
    }

    return 0;
}