#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint32_t version;
} TxVersion;

typedef struct {
    uint64_t value;
    struct hm_bytes scriptPubKey;
} TxOut;

typedef struct {
    struct hm_bytes prevOutHash;
    uint32_t prevOutIndex;
    struct hm_bytes scriptSig;
    uint32_t sequence;
} TxIn;

typedef struct {
    TxVersion version;
    uint64_t txinCount;
    TxIn* txins;
    uint64_t txoutCount;
    TxOut* txouts;
    uint32_t lockTime;
} BitcoinTransaction;

static hm_parser_t* parse_uint32(hm_arena_t* arena) {
    return hm_uint32(arena);
}

static hm_parser_t* parse_uint64(hm_arena_t* arena) {
    return hm_uint64(arena);
}

static hm_parser_t* parse_varint(hm_arena_t* arena) {
    return hm_varint(arena);
}

static hm_parser_t* parse_bytes(hm_arena_t* arena, size_t len) {
    return hm_bytes(arena, len);
}

static hm_parser_t* parse_txin(hm_arena_t* arena) {
    return hm_seq(arena,
                  hm_field(arena, "prevOutHash", hm_bytes(arena, 32)),
                  hm_field(arena, "prevOutIndex", parse_uint32(arena)),
                  hm_field(arena, "scriptSig", hm_varbytes(arena)),
                  hm_field(arena, "sequence", parse_uint32(arena)),
                  NULL);
}

static hm_parser_t* parse_txout(hm_arena_t* arena) {
    return hm_seq(arena,
                  hm_field(arena, "value", parse_uint64(arena)),
                  hm_field(arena, "scriptPubKey", hm_varbytes(arena)),
                  NULL);
}

static hm_parser_t* parse_bitcoin_transaction(hm_arena_t* arena) {
    return hm_seq(arena,
                  hm_field(arena, "version", parse_uint32(arena)),
                  hm_field(arena, "txinCount", parse_varint(arena)),
                  hm_array(arena, "txins", parse_txin(arena), hm_get_uint64(arena, "txinCount")),
                  hm_field(arena, "txoutCount", parse_varint(arena)),
                  hm_array(arena, "txouts", parse_txout(arena), hm_get_uint64(arena, "txoutCount")),
                  hm_field(arena, "lockTime", parse_uint32(arena)),
                  NULL);
}

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

    hm_arena_t* arena = hm_arena_new(1024);
    hm_parser_t* parser = parse_bitcoin_transaction(arena);
    hm_result_t* result = hm_parse(parser, buffer, fileSize);

    if (result->status == HM_SUCCESS) {
        BitcoinTransaction* tx = (BitcoinTransaction*)result->value;
        printf("Transaction Version: %u\n", tx->version.version);
        // Access other fields similarly...  Remember to handle dynamic arrays (txins, txouts) correctly

    } else {
        fprintf(stderr, "Parsing failed: %s\n", result->error);
    }

    hm_result_free(result);
    hm_parser_free(parser);
    hm_arena_free(arena);
    free(buffer);

    return 0;
}
