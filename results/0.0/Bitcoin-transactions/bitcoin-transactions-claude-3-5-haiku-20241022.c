#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t* prev_tx_hash;
    uint32_t prev_output_index;
    uint8_t* script_sig;
    size_t script_sig_len;
    uint32_t sequence_number;
} BitcoinInput;

typedef struct {
    uint64_t amount;
    uint8_t* script_pubkey;
    size_t script_pubkey_len;
} BitcoinOutput;

typedef struct {
    uint32_t version;
    BitcoinInput* inputs;
    size_t input_count;
    BitcoinOutput* outputs;
    size_t output_count;
    uint32_t locktime;
} BitcoinTransaction;

HParser* bitcoin_input_parser() {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),
        h_uint32(),
        h_length_value(h_int_range(h_uint8(), 0, 100), h_bits(8)),
        h_uint32(),
        NULL
    );
}

HParser* bitcoin_output_parser() {
    return h_sequence(
        h_uint64(),
        h_length_value(h_int_range(h_uint8(), 0, 100), h_bits(8)),
        NULL
    );
}

HParser* bitcoin_transaction_parser() {
    return h_sequence(
        h_uint32(),
        h_length_value(h_int_range(h_uint8(), 0, 100), h_many(bitcoin_input_parser())),
        h_length_value(h_int_range(h_uint8(), 0, 100), h_many(bitcoin_output_parser())),
        h_uint32(),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <bitcoin_transaction_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = bitcoin_transaction_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("Transaction parsed successfully\n");
    } else {
        printf("Transaction parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}