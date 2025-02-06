#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint32_t version;
} TxVersion;

typedef struct {
    uint8_t hash[32];
    uint32_t index;
    uint8_t *scriptSig;
    size_t scriptSigLen;
    uint32_t sequence;
} TxIn;

typedef struct {
    uint64_t value;
    uint8_t *scriptPubKey;
    size_t scriptPubKeyLen;
} TxOut;

typedef struct {
    TxVersion version;
    TxIn *inputs;
    size_t numInputs;
    TxOut *outputs;
    size_t numOutputs;
    uint32_t lockTime;
} BitcoinTransaction;

static HParser* parse_uint32(void) {
    return hp_uint32_le();
}

static HParser* parse_uint64(void) {
    return hp_uint64_le();
}

static HParser* parse_bytes(size_t len) {
    return hp_bytes(len);
}

static HParser* parse_varint(void) {
    return hp_varint();
}

static HParser* parse_txin(void) {
    return hp_seq(
        hp_map(parse_bytes(32), NULL),
        parse_uint32(),
        hp_map(parse_varint(), (HMapFunc)malloc),
        hp_map(hp_bytes_n(NULL), NULL),
        parse_uint32()
    );
}

static HParser* parse_txout(void) {
    return hp_seq(
        parse_uint64(),
        hp_map(parse_varint(), (HMapFunc)malloc),
        hp_map(hp_bytes_n(NULL), NULL)
    );
}

static HParser* parse_bitcoin_transaction(void) {
    return hp_seq(
        hp_map(parse_uint32(), NULL),
        hp_map(parse_varint(), (HMapFunc)malloc),
        hp_many_n(parse_txin(), (HMapFunc)malloc),
        hp_map(parse_varint(), (HMapFunc)malloc),
        hp_many_n(parse_txout(), (HMapFunc)malloc),
        parse_uint32()
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    HParser *parser = parse_bitcoin_transaction();
    HResult result = hp_parse(parser, buffer, fileSize);

    if (result.status == H_SUCCESS) {
        BitcoinTransaction *tx = (BitcoinTransaction*)result.value;
        printf("Transaction parsed successfully!\n");
        //Free dynamically allocated memory.  This is crucial and was missing before.
        if (tx->inputs) {
            for (size_t i = 0; i < tx->numInputs; i++) {
                free(tx->inputs[i].scriptSig);
            }
            free(tx->inputs);
        }
        if (tx->outputs) {
            for (size_t i = 0; i < tx->numOutputs; i++) {
                free(tx->outputs[i].scriptPubKey);
            }
            free(tx->outputs);
        }
        free(tx);
    } else {
        fprintf(stderr, "Parsing failed: %s\n", result.error);
    }

    free(buffer);
    hp_free(parser);
    return 0;
}
