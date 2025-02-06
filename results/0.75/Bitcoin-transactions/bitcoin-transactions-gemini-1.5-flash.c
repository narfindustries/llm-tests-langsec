#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint32_t version;
} TransactionHeader;

typedef struct {
    uint8_t hash[32];
    uint32_t index;
    uint8_t scriptSigLen;
    uint8_t* scriptSig;
    uint32_t sequence;
} TransactionInput;

typedef struct {
    uint64_t value;
    uint8_t scriptPubKeyLen;
    uint8_t* scriptPubKey;
} TransactionOutput;

typedef struct {
    TransactionHeader header;
    uint32_t numInputs;
    TransactionInput* inputs;
    uint32_t numOutputs;
    TransactionOutput* outputs;
    uint32_t lockTime;
    uint8_t witnessLen;
    uint8_t** witness;
} BitcoinTransaction;

static HParser* parse_uint32_le(void) {
    return h_uint32_le();
}

static HParser* parse_uint64_le(void) {
    return h_uint64_le();
}

static HParser* parse_bytes(size_t len) {
    return h_bytes(len);
}

static HParser* parse_varint(void) {
    return h_varint();
}

static HParser* parse_transaction_input(void) {
    return h_seq(
            h_map(h_arr(h_byte(), 32), (void*)memcpy),
            parse_uint32_le(),
            parse_varint,
            h_map(h_arr(h_byte(), h_int_val(0)), (void*)memcpy),
            parse_uint32_le(),
            (HParser*)NULL
    );
}

static HParser* parse_transaction_output(void) {
    return h_seq(
            parse_uint64_le(),
            parse_varint,
            h_map(h_arr(h_byte(), h_int_val(0)), (void*)memcpy),
            (HParser*)NULL
    );
}

static HParser* parse_bitcoin_transaction(void) {
    return h_seq(
            parse_uint32_le(),
            parse_varint,
            h_arr(parse_transaction_input(), h_int_val(0)),
            parse_varint,
            h_arr(parse_transaction_output(), h_int_val(0)),
            parse_uint32_le(),
            (HParser*)NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, fileSize, file);
    fclose(file);

    HParser* parser = parse_bitcoin_transaction();
    HParseResult* result = h_parse(parser, buffer, fileSize);

    if (result->status != H_STATUS_SUCCESS) {
        fprintf(stderr, "Parsing failed: %s\n", result->error);
        h_result_free(result);
        free(buffer);
        return 1;
    }

    BitcoinTransaction* transaction = (BitcoinTransaction*)result->value;

    h_result_free(result);
    free(buffer);
    return 0;
}

