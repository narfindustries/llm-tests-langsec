#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static HParser* parse_varint() {
    return h_choice(
        h_uint8(),
        h_uint16(),
        h_uint32(),
        h_uint64(),
        NULL
    );
}

static HParser* parse_tx_input() {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),  // Previous transaction hash
        h_uint32(),                 // Previous output index
        parse_varint(),             // ScriptSig length
        h_many(h_uint8()),          // ScriptSig
        h_uint32(),                 // Sequence number
        NULL
    );
}

static HParser* parse_tx_output() {
    return h_sequence(
        h_int64(),         // Amount
        parse_varint(),    // ScriptPubKey length
        h_many(h_uint8()), // ScriptPubKey
        NULL
    );
}

static HParser* parse_bitcoin_transaction() {
    return h_sequence(
        h_uint32(),        // Version
        parse_varint(),    // Input count
        h_many(parse_tx_input()),  // Inputs
        parse_varint(),    // Output count
        h_many(parse_tx_output()),  // Outputs
        h_uint32(),        // Locktime
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <transaction_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = parse_bitcoin_transaction();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("Transaction parsed successfully\n");
    } else {
        printf("Transaction parsing failed\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}