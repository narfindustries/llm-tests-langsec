#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// Helper function to read binary file into memory
uint8_t* read_file(const char* filename, size_t* length) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("fopen");
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    *length = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    uint8_t* buffer = malloc(*length);
    if (!buffer) {
        perror("malloc");
        fclose(file);
        return NULL;
    }
    
    fread(buffer, 1, *length, file);
    fclose(file);
    return buffer;
}

// Define parsers for Bitcoin transaction fields
HParser* var_int_parser() {
    return h_choice(
        h_uint8(),
        h_sequence(h_const(h_uint8(), 0xFD), h_uint16(), NULL),
        h_sequence(h_const(h_uint8(), 0xFE), h_uint32(), NULL),
        h_sequence(h_const(h_uint8(), 0xFF), h_uint64(), NULL),
        NULL);
}

HParser* hash_parser() {
    return h_repeat_n(h_uint8(), 32);
}

HParser* script_parser() {
    return h_bind(var_int_parser(), h_repeat_n, h_uint8());
}

HParser* input_parser() {
    return h_sequence(
        hash_parser(),       // Previous Transaction Hash
        h_uint32(),          // Output Index
        script_parser(),     // ScriptSig
        h_uint32(),          // Sequence
        NULL);
}

HParser* output_parser() {
    return h_sequence(
        h_uint64(),          // Value
        script_parser(),     // ScriptPubKey
        NULL);
}

HParser* transaction_parser() {
    return h_sequence(
        h_uint32(),          // Version
        h_many(input_parser()), // Inputs
        h_many(output_parser()), // Outputs
        h_uint32(),          // LockTime
        NULL);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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