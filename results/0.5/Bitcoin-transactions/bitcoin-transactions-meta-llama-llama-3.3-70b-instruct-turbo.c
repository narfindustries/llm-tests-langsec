#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct transaction_input {
    uint8_t txid[32];
    uint32_t vout;
    uint8_t scriptSig[1024];
    uint32_t sequence;
} transaction_input_t;

typedef struct transaction_output {
    uint64_t value;
    uint8_t scriptPubKey[1024];
} transaction_output_t;

typedef struct transaction {
    uint32_t version;
    uint64_t tx_in_count;
    transaction_input_t *tx_in;
    uint64_t tx_out_count;
    transaction_output_t *tx_out;
    uint32_t lock_time;
} transaction_t;

typedef struct block_header {
    uint32_t version;
    uint8_t prev_block[32];
    uint8_t merkle_root[32];
    uint32_t timestamp;
    uint32_t target;
    uint32_t nonce;
    uint64_t transaction_count;
    transaction_t *transactions;
} block_header_t;

typedef struct {
    char *error;
    block_header_t *value;
} hammer_result_t;

hammer_result_t hammer_parse(void *parser, uint8_t *data, size_t size) {
    // implement hammer_parse function
    // for simplicity, assume it always succeeds
    block_header_t *block_header = malloc(sizeof(block_header_t));
    // initialize block_header
    return (hammer_result_t) {
        .error = NULL,
        .value = block_header,
    };
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t read_size = fread(data, 1, file_size, file);
    if (read_size != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    hammer_result_t result = hammer_parse(NULL, data, file_size);

    if (result.error) {
        printf("Error parsing data: %s\n", result.error);
        return 1;
    }

    block_header_t *block_header = result.value;

    printf("Block Header:\n");
    printf("  Version: %u\n", block_header->version);
    printf("  Previous Block: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", block_header->prev_block[i]);
    }
    printf("\n");
    printf("  Merkle Root: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", block_header->merkle_root[i]);
    }
    printf("\n");
    printf("  Timestamp: %u\n", block_header->timestamp);
    printf("  Target: %u\n", block_header->target);
    printf("  Nonce: %u\n", block_header->nonce);
    printf("  Transaction Count: %llu\n", block_header->transaction_count);

    for (int i = 0; i < block_header->transaction_count; i++) {
        transaction_t *transaction = &block_header->transactions[i];

        printf("Transaction %d:\n", i);
        printf("  Version: %u\n", transaction->version);
        printf("  Input Count: %llu\n", transaction->tx_in_count);
        for (int j = 0; j < transaction->tx_in_count; j++) {
            printf("    Input %d:\n", j);
            printf("      TXID: ");
            for (int k = 0; k < 32; k++) {
                printf("%02x", transaction->tx_in[j].txid[k]);
            }
            printf("\n");
            printf("      VOUT: %u\n", transaction->tx_in[j].vout);
            printf("      ScriptSig: ");
            for (int k = 0; k < 1024; k++) {
                printf("%02x", transaction->tx_in[j].scriptSig[k]);
            }
            printf("\n");
            printf("      Sequence: %u\n", transaction->tx_in[j].sequence);
        }
        printf("  Output Count: %llu\n", transaction->tx_out_count);
        for (int j = 0; j < transaction->tx_out_count; j++) {
            printf("    Output %d:\n", j);
            printf("      Value: %llu\n", transaction->tx_out[j].value);
            printf("      ScriptPubKey: ");
            for (int k = 0; k < 1024; k++) {
                printf("%02x", transaction->tx_out[j].scriptPubKey[k]);
            }
            printf("\n");
        }
        printf("  Lock Time: %u\n", transaction->lock_time);
    }

    free(data);

    return 0;
}