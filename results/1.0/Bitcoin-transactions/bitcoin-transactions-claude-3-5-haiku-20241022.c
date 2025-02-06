#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    HBytes* previous_tx_hash;
    uint32_t previous_output_index;
    HBytes* script_sig;
    uint32_t sequence_number;
} TransactionInput;

typedef struct {
    uint64_t amount;
    HBytes* script_pubkey;
} TransactionOutput;

typedef struct {
    uint32_t version;
    HPtrArray* inputs;
    HPtrArray* outputs;
    uint32_t locktime;
    
    uint8_t marker;
    uint8_t flag;
    HPtrArray* witness_data;
} BitcoinTransaction;

static HParser* previous_tx_hash_parser() {
    return h_repeat_n(h_ch(0), 32);
}

static HParser* varint_parser() {
    return h_choice(
        h_uint8(),
        h_uint16(),
        h_uint32(),
        h_uint64(),
        NULL
    );
}

static HParser* script_parser() {
    return h_length_value(varint_parser(), h_many(h_ch(0)));
}

static HParser* transaction_input_parser() {
    return h_sequence(
        previous_tx_hash_parser(),
        h_uint32(),
        script_parser(),
        h_uint32(),
        NULL
    );
}

static HParser* transaction_output_parser() {
    return h_sequence(
        h_uint64(),
        script_parser(),
        NULL
    );
}

static HParser* bitcoin_transaction_parser() {
    HParser* standard_parser = h_sequence(
        h_uint32(),
        h_length_value(varint_parser(), transaction_input_parser()),
        h_length_value(varint_parser(), transaction_output_parser()),
        h_uint32(),
        NULL
    );
    
    HParser* segwit_parser = h_sequence(
        h_ch(0x00),
        h_ch(0x01),
        h_uint32(),
        h_length_value(varint_parser(), transaction_input_parser()),
        h_length_value(varint_parser(), transaction_output_parser()),
        h_length_value(varint_parser(), h_length_value(varint_parser(), h_ch(0))),
        h_uint32(),
        NULL
    );
    
    return h_choice(standard_parser, segwit_parser, NULL);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <transaction_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
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

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = bitcoin_transaction_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("Transaction parsed successfully\n");
    } else {
        printf("Transaction parsing failed\n");
    }

    free(buffer);
    h_parse_result_free(result);
    h_parser_free(parser);

    return 0;
}