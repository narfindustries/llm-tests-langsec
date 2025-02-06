#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint32_t version;
    uint64_t input_count;
    void *inputs;
    uint64_t output_count;
    void *outputs;
    uint32_t lock_time;
} BitcoinTransaction;

HParser *parse_version() {
    return h_uint32();
}

HParser *parse_varint() {
    uint8_t fd = 0xFD, fe = 0xFE, ff = 0xFF, zero = 0x00;
    return h_choice(h_sequence(h_token(&fd, 1), h_uint16(), NULL),
                    h_sequence(h_token(&fe, 1), h_uint32(), NULL),
                    h_sequence(h_token(&ff, 1), h_uint64(), NULL),
                    h_sequence(h_token(&zero, 1), h_uint8(), NULL),
                    h_uint8(), NULL);
}

HParser *parse_input() {
    return h_sequence(h_bits(32, 8, NULL), // Previous Transaction Hash
                      h_uint32(),          // Previous Transaction Output Index
                      parse_varint(),       // Script Length
                      h_bits(h_length(), 8, NULL), // Unlocking Script
                      h_uint32(),          // Sequence Number
                      NULL);
}

HParser *parse_output() {
    return h_sequence(h_uint64(),          // Value
                      parse_varint(),       // Script Length
                      h_bits(h_length(), 8, NULL), // Locking Script
                      NULL);
}

HParser *parse_bitcoin_transaction() {
    return h_sequence(parse_version(),     // Version
                      parse_varint(),       // Input Count
                      h_repeat_n(parse_input(), h_length()), // Inputs
                      parse_varint(),      // Output Count
                      h_repeat_n(parse_output(), h_length()), // Outputs
                      h_uint32(),          // Lock Time
                      NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Error allocating memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = parse_bitcoin_transaction();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Error parsing Bitcoin transaction\n");
        free(buffer);
        return 1;
    }

    BitcoinTransaction *tx = (BitcoinTransaction *)result->ast;
    printf("Version: %u\n", tx->version);
    printf("Input Count: %lu\n", tx->input_count);
    printf("Output Count: %lu\n", tx->output_count);
    printf("Lock Time: %u\n", tx->lock_time);

    h_parse_result_free(result);
    free(buffer);
    return 0;
}