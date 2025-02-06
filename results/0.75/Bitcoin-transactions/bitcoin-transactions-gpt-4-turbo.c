#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// Function to parse a variable length integer
static HParser *var_int() {
    return h_choice(h_uint64(), h_uint32(), h_uint16(), h_uint8(), NULL);
}

// Function to parse the transaction input
static HParser *tx_input() {
    HParser *prev_tx_hash = h_bits(256, false);
    HParser *prev_tx_out_index = h_uint32();
    HParser *script_length = var_int();
    HParser *script_sig = h_bits(h_length_value(script_length, h_uint8()), false);
    HParser *sequence_number = h_uint32();

    return h_sequence(prev_tx_hash, prev_tx_out_index, script_sig, sequence_number, NULL);
}

// Function to parse the transaction output
static HParser *tx_output() {
    HParser *value = h_uint64();
    HParser *script_length = var_int();
    HParser *script_pub_key = h_bits(h_length_value(script_length, h_uint8()), false);

    return h_sequence(value, script_pub_key, NULL);
}

// Function to parse a full Bitcoin transaction
static HParser *bitcoin_tx() {
    HParser *version = h_uint32();
    HParser *input_count = var_int();
    HParser *inputs = h_many(tx_input());
    HParser *output_count = var_int();
    HParser *outputs = h_many(tx_output());
    HParser *lock_time = h_uint32();

    return h_sequence(version, h_length_value(input_count, inputs), h_length_value(output_count, outputs), lock_time, NULL);
}

// Main program
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <bitcoin_tx_file>\n", argv[0]);
        return 1;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to find the file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Read the entire file into memory
    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return 1;
    }

    // Parse the binary data
    HParser *tx_parser = bitcoin_tx();
    HParseResult *result = h_parse(tx_parser, data, file_size);
    if (result) {
        printf("Transaction parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "Failed to parse transaction.\n");
    }

    // Clean up
    h_parse_result_free(result);
    free(data);
    fclose(file);
    return 0;
}