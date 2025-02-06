#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function prototypes
static void parse_bitcoin_transaction(const uint8_t *buffer, size_t length);

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary transaction file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    parse_bitcoin_transaction(buffer, file_size);
    free(buffer);

    return EXIT_SUCCESS;
}

static void parse_bitcoin_transaction(const uint8_t *buffer, size_t length) {
    HParser *uint32 = h_uint32();
    HParser *uint64 = h_uint64();
    HParser *var_int = h_uint64(); // Varint parser using uint64 directly
    HParser *bits256 = h_bits(256, false);
    HParser *bits32 = h_bits(32, false);
    HParser *tx_in_previous_output = h_sequence(bits256, bits32, NULL);
    HParser *tx_in_seq = uint32;

    HParser *bytes = h_blob(); // Using h_blob() as a placeholder since h_bytes is not defined in Hammer
    HParser *tx_input = h_sequence(tx_in_previous_output, h_length_value(var_int, bytes), tx_in_seq, NULL);

    HParser *tx_out_value = uint64;
    HParser *tx_out_script = h_length_value(var_int, bytes);
    HParser *tx_output = h_sequence(tx_out_value, tx_out_script, NULL);

    HParser *transaction = h_sequence(uint32,
                                      h_length_value(var_int, tx_input),
                                      h_length_value(var_int, tx_output),
                                      uint32,
                                      NULL);

    HParseResult *result = h_parse(transaction, buffer, length);
    if (result && result->result) {
        printf("Transaction parsed successfully.\n");
        h_pprint(stdout, result, 0, 0);
    } else {
        printf("Failed to parse transaction.\n");
    }

    h_parser_free(transaction);
    h_parser_free(tx_input);
    h_parser_free(tx_output);
    h_parse_result_free(result);
}