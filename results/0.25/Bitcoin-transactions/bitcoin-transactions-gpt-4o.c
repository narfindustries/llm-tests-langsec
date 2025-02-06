#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Function to read file into memory
uint8_t* read_file(const char* filename, size_t* length) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    *length = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t* buffer = (uint8_t*)malloc(*length);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return NULL;
    }
    fread(buffer, 1, *length, file);
    fclose(file);
    return buffer;
}

// Define parsers for Bitcoin transaction fields
HParser* varint_parser() {
    return h_choice(
        h_sequence(h_uint8(), NULL),
        h_sequence(h_uint16(), NULL),
        h_sequence(h_uint32(), NULL),
        h_sequence(h_uint64(), NULL),
        NULL
    );
}

HParser* hash_parser() {
    return h_repeat_n(h_uint8(), 32);
}

HParser* script_parser() {
    return h_length_value(varint_parser(), h_uint8());
}

HParser* input_parser() {
    return h_sequence(
        hash_parser(), // Previous Transaction Hash
        h_uint32(),    // Output Index
        script_parser(), // ScriptSig
        h_uint32(),    // Sequence
        NULL
    );
}

HParser* output_parser() {
    return h_sequence(
        h_uint64(),    // Value
        script_parser(), // ScriptPubKey
        NULL
    );
}

HParser* transaction_parser() {
    return h_sequence(
        h_uint32(),    // Version
        h_length_value(varint_parser(), input_parser()), // Inputs
        h_length_value(varint_parser(), output_parser()), // Outputs
        h_uint32(),    // Locktime
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    size_t length;
    uint8_t* data = read_file(argv[1], &length);
    if (!data) {
        return EXIT_FAILURE;
    }

    HParser* parser = transaction_parser();
    HParseResult* result = h_parse(parser, data, length);

    if (result) {
        printf("Transaction parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse transaction.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}