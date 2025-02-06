#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

HParser *bitcoin_parser() {
    HParser *version = h_int32();
    HParser *input_count = h_uint32();
    HParser *input = h_sequence(h_bits(32, false), h_uint32(), h_length_value(h_uint32(), h_bits(8, false)), h_uint32(), NULL);
    HParser *inputs = h_repeat_n(input_count, input);
    HParser *output_count = h_uint32();
    HParser *output = h_sequence(h_uint64(), h_length_value(h_uint32(), h_bits(8, false)), NULL);
    HParser *outputs = h_repeat_n(output_count, output);
    HParser *lock_time = h_uint32();
    HParser *witness_count = h_uint32();
    HParser *witness_element = h_length_value(h_uint32(), h_bits(8, false));
    HParser *witness = h_repeat_n(witness_count, witness_element);
    HParser *transaction = h_sequence(version, input_count, inputs, output_count, outputs, lock_time, h_optional(witness), NULL);
    return transaction;
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

    HParser *parser = bitcoin_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse transaction\n");
        free(buffer);
        return 1;
    }

    printf("Transaction parsed successfully\n");
    h_parse_result_free(result);
    free(buffer);
    return 0;
}