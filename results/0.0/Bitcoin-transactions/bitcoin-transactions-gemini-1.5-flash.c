#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define Bitcoin data types
typedef struct {
    uint32_t version;
    uint32_t locktime;
} BitcoinHeader;

typedef struct {
    uint8_t hash[32];
    uint32_t index;
} TxInPrevOut;

typedef struct {
    TxInPrevOut prev_out;
    uint8_t* scriptSig;
    size_t scriptSigLen;
    uint32_t sequence;
} BitcoinTxIn;

typedef struct {
    uint64_t value;
    uint8_t* scriptPubKey;
    size_t scriptPubKeyLen;
} BitcoinTxOut;

typedef struct {
    BitcoinHeader header;
    BitcoinTxIn* txins;
    size_t txinCount;
    BitcoinTxOut* txouts;
    size_t txoutCount;
    uint8_t* witness;
    size_t witnessLen;
} BitcoinTransaction;

// Helper function to allocate and copy bytes
static uint8_t* copy_bytes(const uint8_t* data, size_t len) {
    uint8_t* copy = (uint8_t*)malloc(len);
    if (copy == NULL) {
        perror("Memory allocation failed");
        exit(1);
    }
    memcpy(copy, data, len);
    return copy;
}

// Helper function to free allocated memory for BitcoinTxIn array
static void free_txin_array(BitcoinTxIn* txins, size_t count) {
    if (txins == NULL) return; //Handle potential NULL pointer
    for (size_t i = 0; i < count; ++i) {
        free(txins[i].scriptSig);
    }
    free(txins);
}

// Helper function to free allocated memory for BitcoinTxOut array
static void free_txout_array(BitcoinTxOut* txouts, size_t count) {
    if (txouts == NULL) return; //Handle potential NULL pointer
    for (size_t i = 0; i < count; ++i) {
        free(txouts[i].scriptPubKey);
    }
    free(txouts);
}


// Hammer parser combinators for Bitcoin transaction fields
static HammerParser uint32Parser = hammer_uint32_le;
static HammerParser uint64Parser = hammer_uint64_le;
static HammerParser hash256Parser = hammer_bytes(32);

static HammerParser txInPrevOutParser = hammer_map(
    hammer_tuple2(hash256Parser, hammer_uint32_le),
    hammer_construct(TxInPrevOut, .hash = hammer_field_at(0), .index = hammer_field_at(1))
);

static HammerParser varIntParser = hammer_varint;

static HammerParser scriptParser = hammer_map(
    hammer_tuple2(varIntParser, hammer_bytes),
    hammer_construct_with(uint8_t*, copy_bytes, hammer_field_at(1), hammer_field_at(0))
);


static HammerParser txInParser = hammer_map(
    hammer_tuple4(txInPrevOutParser, scriptParser, uint32Parser, hammer_uint32_le),
    hammer_construct(BitcoinTxIn, .prev_out = hammer_field_at(0), .scriptSig = hammer_field_at(1), .scriptSigLen = hammer_field_at(1), .sequence = hammer_field_at(2))
);

static HammerParser txOutParser = hammer_map(
    hammer_tuple2(uint64Parser, scriptParser),
    hammer_construct(BitcoinTxOut, .value = hammer_field_at(0), .scriptPubKey = hammer_field_at(1), .scriptPubKeyLen = hammer_field_at(1))
);

static HammerParser txInListParser = hammer_map(
    hammer_tuple2(varIntParser, hammer_array(txInParser)),
    hammer_construct_with(BitcoinTxIn*, hammer_array_to_ptr, hammer_field_at(1), hammer_field_at(0))
);

static HammerParser txOutListParser = hammer_map(
    hammer_tuple2(varIntParser, hammer_array(txOutParser)),
    hammer_construct_with(BitcoinTxOut*, hammer_array_to_ptr, hammer_field_at(1), hammer_field_at(0))
);

static HammerParser witnessParser = hammer_map(
    hammer_tuple2(varIntParser, hammer_bytes),
    hammer_construct_with(uint8_t*, copy_bytes, hammer_field_at(1), hammer_field_at(0))
);

static HammerParser bitcoinTransactionParser = hammer_map(
    hammer_tuple5(uint32Parser, txInListParser, txOutListParser, uint32Parser, witnessParser),
    hammer_construct(BitcoinTransaction, .header.version = hammer_field_at(0), .txins = hammer_field_at(1), .txinCount = hammer_field_at(1), .txouts = hammer_field_at(2), .txoutCount = hammer_field_at(2), .witness = hammer_field_at(4), .witnessLen = hammer_field_at(4), .header.locktime = hammer_field_at(3))
);


int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    HammerResult result = hammer_parse(bitcoinTransactionParser, buffer, fileSize);

    if (result.success) {
        printf("Transaction parsed successfully!\n");
        // Access parsed data here (result.value) -  requires proper struct handling
        BitcoinTransaction* tx = (BitcoinTransaction*)result.value;
        free_txin_array(tx->txins, tx->txinCount);
        free_txout_array(tx->txouts, tx->txoutCount);
        free(tx->witness);
        free(tx);

    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}