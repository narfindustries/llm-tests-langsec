#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser for a variable-length integer
static HParser *var_int() {
    return h_choice(h_int64(), h_int32(), h_int16(), h_int8(), NULL);
}

// Parser for a Bitcoin transaction input
static HParser *tx_input() {
    HParser *prev_tx_hash = h_bits(256, false);
    HParser *output_index = h_uint32();
    HParser *script_length = var_int();
    HParser *script_sig = h_length_value(script_length, h_bits(h_uint_value(script_length), false));
    HParser *sequence = h_uint32();

    return h_sequence(prev_tx_hash, output_index, script_sig, sequence, NULL);
}

// Parser for a Bitcoin transaction output
static HParser *tx_output() {
    HParser *value = h_uint64();
    HParser *script_length = var_int();
    HParser *script_pubkey = h_length_value(script_length, h_bits(h_uint_value(script_length), false));

    return h_sequence(value, script_pubkey, NULL);
}

// Main parser for a Bitcoin transaction
static HParser *bitcoin_tx() {
    HParser *version = h_uint32();
    HParser *input_count = var_int();
    HParser *inputs = h_many1(tx_input());
    HParser *output_count = var_int();
    HParser *outputs = h_many1(tx_output());
    HParser *locktime = h_uint32();

    return h_sequence(version, input_count, inputs, output_count, outputs, locktime, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <bitcoin_tx_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buf = malloc(fsize);
    fread(buf, 1, fsize, fp);
    fclose(fp);

    HParser *btc_parser = bitcoin_tx();
    HParseResult *result = h_parse(btc_parser, buf, fsize);

    if (result) {
        printf("Parse successful!\n");
    } else {
        printf("Parse failed!\n");
    }

    h_parse_result_free(result);
    h_parser_free(btc_parser);
    free(buf);

    return 0;
}