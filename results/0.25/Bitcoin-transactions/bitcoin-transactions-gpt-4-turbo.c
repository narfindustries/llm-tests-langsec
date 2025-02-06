#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define Bitcoin transaction structures
typedef struct {
    uint8_t hash[32];
    uint32_t index;
} tx_in_outpoint;

typedef struct {
    tx_in_outpoint outpoint;
    HBytes *scriptSig;
    uint32_t sequence;
} tx_input;

typedef struct {
    uint64_t value;
    HBytes *scriptPubKey;
} tx_output;

typedef struct {
    uint32_t version;
    uint64_t input_count;
    tx_input *inputs;
    uint64_t output_count;
    tx_output *outputs;
    uint32_t locktime;
} bitcoin_transaction;

// Hammer parsers for Bitcoin transaction components
HParser *parse_uint32() {
    return h_uint32();
}

HParser *parse_uint64() {
    return h_uint64();
}

HParser *parse_outpoint() {
    return h_sequence(h_bits(256, false), parse_uint32(), NULL);
}

HParser *parse_input() {
    return h_sequence(parse_outpoint(), h_length_value(h_uint64(), h_bits(0, false)), parse_uint32(), NULL);
}

HParser *parse_output() {
    return h_sequence(parse_uint64(), h_length_value(h_uint64(), h_bits(0, false)), NULL);
}

HParser *parse_transaction() {
    return h_sequence(
        parse_uint32(),
        h_length_value(h_uint64(), parse_input()),
        h_length_value(h_uint64(), parse_output()),
        parse_uint32(),
        NULL
    );
}

// Main function to parse a Bitcoin transaction from a binary file
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary transaction file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buf = malloc(fsize);
    fread(buf, 1, fsize, fp);
    fclose(fp);

    HParser *transaction_parser = parse_transaction();
    HParseResult *result = h_parse(transaction_parser, buf, fsize);
    if (result) {
        printf("Transaction parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse transaction.\n");
    }

    h_parse_result_free(result);
    free(transaction_parser);
    free(buf);

    return 0;
}