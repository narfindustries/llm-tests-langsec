#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_SCRIPT_SIZE 10000
#define MAX_INPUTS 1000
#define MAX_OUTPUTS 1000
#define MAX_WITNESS_COUNT 1000

typedef struct {
    uint8_t previous_tx_hash[32];
    uint32_t previous_output_index;
    uint8_t *script_sig;
    uint32_t script_sig_len;
    uint32_t sequence;
} TransactionInput;

typedef struct {
    uint64_t value;
    uint8_t *script_pubkey;
    uint32_t script_pubkey_len;
} TransactionOutput;

typedef struct {
    uint32_t version;
    TransactionInput *inputs;
    uint32_t inputs_len;
    TransactionOutput *outputs;
    uint32_t outputs_len;
    uint32_t lock_time;
    uint8_t *witness;
    uint32_t witness_len;
} Transaction;

HParser *parse_transaction_input() {
    return h_sequence(
        h_bits(256, NULL), // previous_tx_hash (32 bytes)
        h_uint32(),        // previous_output_index
        h_length_value(h_uint32(), h_bits(MAX_SCRIPT_SIZE * 8, NULL)), // script_sig
        h_uint32(),        // sequence
        NULL
    );
}

HParser *parse_transaction_output() {
    return h_sequence(
        h_uint64(),        // value
        h_length_value(h_uint32(), h_bits(MAX_SCRIPT_SIZE * 8, NULL)), // script_pubkey
        NULL
    );
}

HParser *parse_transaction() {
    return h_sequence(
        h_uint32(),        // version
        h_length_value(h_uint32(), h_repeat_n(parse_transaction_input(), MAX_INPUTS)), // inputs
        h_length_value(h_uint32(), h_repeat_n(parse_transaction_output(), MAX_OUTPUTS)), // outputs
        h_uint32(),        // lock_time
        h_optional(h_length_value(h_uint32(), h_bits(MAX_WITNESS_COUNT * 8, NULL))), // witness
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = parse_transaction();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse transaction\n");
        free(buffer);
        return 1;
    }

    Transaction *tx = (Transaction *)result->ast;
    printf("Transaction Version: %u\n", tx->version);

    h_parse_result_free(result);
    free(buffer);

    return 0;
}