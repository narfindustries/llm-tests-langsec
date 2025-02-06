#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define u8  uint8_t
#define u32 uint32_t
#define u64 uint64_t

typedef struct {
    u32 version;
    u32 lock_time;
    struct txin_t *txin;
    int txin_len;
    struct txout_t *txout;
    int txout_len;
} tx_t;

typedef struct {
    u8 txin_scriptlen;
    u8 *txin_script;
    struct prevout_t *prevout;
} txin_t;

typedef struct {
    u64 value;
    u8 txout_scriptlen;
    u8 *txout_script;
} txout_t;

typedef struct {
    u8 hash[32];
    u32 index;
} prevout_t;

void parse_transaction(tx_t *tx, u8 *buf, int len) {
    int offset = 0;
    tx->version = *((u32*) (buf + offset));
    offset += sizeof(u32);
    tx->lock_time = *((u32*) (buf + offset));
    offset += sizeof(u32);

    tx->txin_len = 1; 
    tx->txin = malloc(sizeof(txin_t) * tx->txin_len);
    for (int i = 0; i < tx->txin_len; i++) {
        tx->txin[i].txin_scriptlen = *(buf + offset);
        offset += sizeof(u8);
        tx->txin[i].txin_script = malloc(tx->txin[i].txin_scriptlen);
        memcpy(tx->txin[i].txin_script, buf + offset, tx->txin[i].txin_scriptlen);
        offset += tx->txin[i].txin_scriptlen;
        tx->txin[i].prevout = malloc(sizeof(prevout_t));
        memcpy(tx->txin[i].prevout->hash, buf + offset, 32);
        offset += 32;
        tx->txin[i].prevout->index = *((u32*) (buf + offset));
        offset += sizeof(u32);
    }

    tx->txout_len = 1; 
    tx->txout = malloc(sizeof(txout_t) * tx->txout_len);
    for (int i = 0; i < tx->txout_len; i++) {
        tx->txout[i].value = *((u64*) (buf + offset));
        offset += sizeof(u64);
        tx->txout[i].txout_scriptlen = *(buf + offset);
        offset += sizeof(u8);
        tx->txout[i].txout_script = malloc(tx->txout[i].txout_scriptlen);
        memcpy(tx->txout[i].txout_script, buf + offset, tx->txout[i].txout_scriptlen);
        offset += tx->txout[i].txout_scriptlen;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <bitcoin binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    u8 *buf = malloc(file_size);
    fread(buf, file_size, 1, file);

    tx_t tx;
    parse_transaction(&tx, buf, file_size);

    printf("Transaction:\n");
    printf("Version: %u\n", tx.version);
    printf("Lock Time: %u\n", tx.lock_time);
    printf("Inputs:\n");
    for (int i = 0; i < tx.txin_len; i++) {
        printf("  Input %d:\n", i);
        printf("    Script Length: %u\n", tx.txin[i].txin_scriptlen);
        printf("    Script: %p\n", tx.txin[i].txin_script);
        printf("    Previous Output:\n");
        printf("      Hash: %p\n", tx.txin[i].prevout->hash);
        printf("      Index: %u\n", tx.txin[i].prevout->index);
    }
    printf("Outputs:\n");
    for (int i = 0; i < tx.txout_len; i++) {
        printf("  Output %d:\n", i);
        printf("    Value: %llu\n", tx.txout[i].value);
        printf("    Script Length: %u\n", tx.txout[i].txout_scriptlen);
        printf("    Script: %p\n", tx.txout[i].txout_script);
    }

    free(buf);
    fclose(file);
    return 0;
}