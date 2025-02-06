#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser for a variable length integer (VarInt)
static HParser *varint() {
    return h_choice(h_int64(), h_int32(), h_int16(), h_int8(), NULL);
}

// Parser for a transaction input
static HParser *tx_input() {
    HParser *prev_tx_hash = h_bits(256, false);
    HParser *prev_tx_index = h_bits(32, false);
    HParser *script_length = varint();
    HParser *script_sig = h_length_value(script_length, h_uint8());
    HParser *sequence = h_bits(32, false);

    return h_sequence(prev_tx_hash, prev_tx_index, script_length, script_sig, sequence, NULL);
}

// Parser for a transaction output
static HParser *tx_output() {
    HParser *value = h_bits(64, false);
    HParser *script_length = varint();
    HParser *script_pubkey = h_length_value(script_length, h_uint8());

    return h_sequence(value, script_length, script_pubkey, NULL);
}

// Parser for a Bitcoin transaction
static HParser *bitcoin_tx() {
    HParser *version = h_bits(32, false);
    HParser *input_count = varint();
    HParser *inputs = h_many(tx_input());
    HParser *output_count = varint();
    HParser *outputs = h_many(tx_output());
    HParser *lock_time = h_bits(32, false);

    return h_sequence(version, input_count, inputs, output_count, outputs, lock_time, NULL);
}

// Main function to parse a Bitcoin transaction from a binary file
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary transaction file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to determine the file size
    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read the entire file into a buffer
    uint8_t *buffer = malloc(length);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, length, file) != length) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    fclose(file);

    // Create a parser for a Bitcoin transaction
    HParser *parser = bitcoin_tx();
    HParseResult *result = h_parse(parser, buffer, length);

    if (result) {
        printf("Transaction parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse transaction.\n");
    }

    // Clean up
    h_parse_result_free(result);
    h_free_parser(parser);
    free(buffer);

    return 0;
}