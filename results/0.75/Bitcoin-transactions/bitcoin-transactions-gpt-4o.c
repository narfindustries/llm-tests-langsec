#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *create_bitcoin_transactions_parser() {
    HParser *varint = h_repeat(h_choice(h_bits(7), h_bits(8), NULL), 1, 5);
    HParser *hash = h_fixed_bytes(32);
    HParser *txid = hash;
    HParser *index = h_uint32();
    HParser *script_length = varint;
    HParser *script_sig = h_length_value(script_length, h_repeat(h_uint8(), 0, UINT_MAX));
    HParser *sequence = h_uint32();
    HParser *input = h_sequence(txid, index, script_sig, sequence, NULL);
    HParser *inputs = h_many(input);

    HParser *value = h_uint64();
    HParser *pk_script_length = varint;
    HParser *pk_script = h_length_value(pk_script_length, h_repeat(h_uint8(), 0, UINT_MAX));
    HParser *output = h_sequence(value, pk_script_length, pk_script, NULL);
    HParser *outputs = h_many(output);

    HParser *lock_time = h_uint32();
    HParser *transaction = h_sequence(varint, inputs, outputs, lock_time, NULL);

    return transaction;
}

void parse_bitcoin_transaction(const uint8_t *data, size_t length) {
    HParser *parser = create_bitcoin_transactions_parser();
    HParseResult *result = h_parse(parser, data, length);
    if (result->succeeded) {
        printf("Transaction parsed successfully\n");
    } else {
        fprintf(stderr, "Failed to parse transaction\n");
    }
    h_parse_result_free(result);
    h_parser_free(parser);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hex_data>\n", argv[0]);
        return EXIT_FAILURE;
    }

    size_t length = strlen(argv[1]) / 2;
    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        return EXIT_FAILURE;
    }

    for (size_t i = 0; i < length; i++) {
        sscanf(argv[1] + 2 * i, "%2hhx", &data[i]);
    }

    parse_bitcoin_transaction(data, length);
    free(data);
    return EXIT_SUCCESS;
}