#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Define structures for Bitcoin transaction components
typedef struct {
    uint8_t prev_tx_hash[32];
    uint32_t output_index;
    uint8_t *script;
    uint32_t sequence;
} TxInput;

typedef struct {
    uint64_t value;
    uint8_t *script;
} TxOutput;

// Define parser functions for Bitcoin transaction
HParser *varint_parser() {
    return h_choice(
        h_uint8(),
        h_right(h_bits(1, false), h_left(h_bits(7, false), h_uint16())),
        h_right(h_bits(3, false), h_left(h_bits(5, false), h_uint32())),
        h_right(h_bits(7, false), h_left(h_bits(1, false), h_uint64())),
        NULL
    );
}

HParser *tx_input_parser() {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),  // Previous Transaction Hash
        h_uint32(),                 // Output Index
        h_bind(varint_parser(), h_data), // Script
        h_uint32(),                 // Sequence
        NULL
    );
}

HParser *tx_output_parser() {
    return h_sequence(
        h_uint64(),                 // Value
        h_bind(varint_parser(), h_data), // Script
        NULL
    );
}

HParser *bitcoin_tx_parser() {
    return h_sequence(
        h_uint32(), // Version
        h_bind(varint_parser(), h_repeat(tx_input_parser(), h_last_uint, NULL)),  // Inputs
        h_bind(varint_parser(), h_repeat(tx_output_parser(), h_last_uint, NULL)), // Outputs
        h_uint32(), // Locktime
        NULL
    );
}

void parse_bitcoin_tx(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = bitcoin_tx_parser();
    HParseResult *result = h_parse(parser, data, file_size);
    
    if (result) {
        printf("Transaction parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse transaction\n");
    }

    free(data);
    h_parser_free(parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_bitcoin_tx(argv[1]);

    return EXIT_SUCCESS;
}