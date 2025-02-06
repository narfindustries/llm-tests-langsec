#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define VARINT_MAX_SIZE 9

typedef struct {
    uint32_t version;
    uint8_t tx_id[32];
    uint64_t num_inputs;
    struct input {
        uint8_t prev_tx_hash[32];
        uint32_t prev_tx_index;
        uint64_t script_length;
        uint8_t* script;
        uint32_t sequence_number;
    }* inputs;
    uint64_t num_outputs;
    struct output {
        uint64_t value;
        uint64_t script_length;
        uint8_t* script;
    }* outputs;
    uint32_t lock_time;
} transaction_t;

typedef struct {
    uint32_t version;
    uint8_t prev_block_hash[32];
    uint8_t merkle_root[32];
    uint32_t timestamp;
    uint32_t target;
    uint32_t nonce;
    uint64_t num_transactions;
    transaction_t* transactions;
} block_t;

void* varint_parser(void* data, size_t* size) {
    uint8_t* bytes = data;
    size_t len = *size;
    if (len < 1) return NULL;
    uint8_t first_byte = bytes[0];
    if (first_byte < 0xFD) {
        *size = 1;
        return (void*)((uint64_t)first_byte);
    } else if (first_byte == 0xFD) {
        if (len < 3) return NULL;
        uint16_t value = (uint16_t)((bytes[1] << 8) | bytes[2]);
        *size = 3;
        return (void*)((uint64_t)value);
    } else if (first_byte == 0xFE) {
        if (len < 5) return NULL;
        uint32_t value = (uint32_t)((bytes[1] << 24) | (bytes[2] << 16) | (bytes[3] << 8) | bytes[4]);
        *size = 5;
        return (void*)((uint64_t)value);
    } else if (first_byte == 0xFF) {
        if (len < 9) return NULL;
        uint64_t value = (uint64_t)((uint64_t)bytes[1] << 56 | (uint64_t)bytes[2] << 48 | (uint64_t)bytes[3] << 40 | (uint64_t)bytes[4] << 32 | (uint64_t)bytes[5] << 24 | (uint64_t)bytes[6] << 16 | (uint64_t)bytes[7] << 8 | bytes[8]);
        *size = 9;
        return (void*)value;
    }
    return NULL;
}

void* transaction_parser(void* data, size_t* size) {
    uint8_t* bytes = data;
    size_t len = *size;
    if (len < 4 + 32 + 1) return NULL;
    uint32_t version = (uint32_t)((bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3]);
    uint8_t tx_id[32];
    memcpy(tx_id, bytes + 4, 32);
    uint64_t num_inputs = (uint64_t)varint_parser(bytes + 36, &len);
    if (num_inputs == 0) return NULL;
    struct input* inputs = malloc(num_inputs * sizeof(struct input));
    size_t offset = 36 + VARINT_MAX_SIZE;
    for (int i = 0; i < num_inputs; i++) {
        if (len < offset + 32 + 4 + 1) return NULL;
        uint8_t prev_tx_hash[32];
        memcpy(prev_tx_hash, bytes + offset, 32);
        uint32_t prev_tx_index = (uint32_t)((bytes[offset + 32] << 24) | (bytes[offset + 33] << 16) | (bytes[offset + 34] << 8) | bytes[offset + 35]);
        uint64_t script_length = (uint64_t)varint_parser(bytes + offset + 36, &len);
        if (script_length == 0) return NULL;
        uint8_t* script = malloc(script_length);
        memcpy(script, bytes + offset + 36 + VARINT_MAX_SIZE, script_length);
        uint32_t sequence_number = (uint32_t)((bytes[offset + 36 + VARINT_MAX_SIZE + script_length] << 24) | (bytes[offset + 36 + VARINT_MAX_SIZE + script_length + 1] << 16) | (bytes[offset + 36 + VARINT_MAX_SIZE + script_length + 2] << 8) | bytes[offset + 36 + VARINT_MAX_SIZE + script_length + 3]);
        memcpy(inputs[i].prev_tx_hash, prev_tx_hash, 32);
        inputs[i].prev_tx_index = prev_tx_index;
        inputs[i].script_length = script_length;
        inputs[i].script = script;
        inputs[i].sequence_number = sequence_number;
        offset += 36 + VARINT_MAX_SIZE + script_length + 4;
    }
    uint64_t num_outputs = (uint64_t)varint_parser(bytes + offset, &len);
    if (num_outputs == 0) return NULL;
    struct output* outputs = malloc(num_outputs * sizeof(struct output));
    offset += VARINT_MAX_SIZE;
    for (int i = 0; i < num_outputs; i++) {
        if (len < offset + 8 + 1) return NULL;
        uint64_t value = (uint64_t)((uint64_t)bytes[offset] << 56 | (uint64_t)bytes[offset + 1] << 48 | (uint64_t)bytes[offset + 2] << 40 | (uint64_t)bytes[offset + 3] << 32 | (uint64_t)bytes[offset + 4] << 24 | (uint64_t)bytes[offset + 5] << 16 | (uint64_t)bytes[offset + 6] << 8 | bytes[offset + 7]);
        uint64_t script_length = (uint64_t)varint_parser(bytes + offset + 8, &len);
        if (script_length == 0) return NULL;
        uint8_t* script = malloc(script_length);
        memcpy(script, bytes + offset + 8 + VARINT_MAX_SIZE, script_length);
        outputs[i].value = value;
        outputs[i].script_length = script_length;
        outputs[i].script = script;
        offset += 8 + VARINT_MAX_SIZE + script_length;
    }
    uint32_t lock_time = (uint32_t)((bytes[offset] << 24) | (bytes[offset + 1] << 16) | (bytes[offset + 2] << 8) | bytes[offset + 3]);
    transaction_t* transaction = malloc(sizeof(transaction_t));
    transaction->version = version;
    memcpy(transaction->tx_id, tx_id, 32);
    transaction->num_inputs = num_inputs;
    transaction->inputs = inputs;
    transaction->num_outputs = num_outputs;
    transaction->outputs = outputs;
    transaction->lock_time = lock_time;
    *size = offset + 4;
    return transaction;
}

void* block_parser(void* data, size_t* size) {
    uint8_t* bytes = data;
    size_t len = *size;
    if (len < 4 + 32 + 32 + 4 + 4 + 4 + 1) return NULL;
    uint32_t version = (uint32_t)((bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3]);
    uint8_t prev_block_hash[32];
    memcpy(prev_block_hash, bytes + 4, 32);
    uint8_t merkle_root[32];
    memcpy(merkle_root, bytes + 36, 32);
    uint32_t timestamp = (uint32_t)((bytes[68] << 24) | (bytes[69] << 16) | (bytes[70] << 8) | bytes[71]);
    uint32_t target = (uint32_t)((bytes[72] << 24) | (bytes[73] << 16) | (bytes[74] << 8) | bytes[75]);
    uint32_t nonce = (uint32_t)((bytes[76] << 24) | (bytes[77] << 16) | (bytes[78] << 8) | bytes[79]);
    uint64_t num_transactions = (uint64_t)varint_parser(bytes + 80, &len);
    if (num_transactions == 0) return NULL;
    transaction_t* transactions = malloc(num_transactions * sizeof(transaction_t));
    size_t offset = 80 + VARINT_MAX_SIZE;
    for (int i = 0; i < num_transactions; i++) {
        transactions[i] = *(transaction_t*)transaction_parser(bytes + offset, &len);
        offset += len;
    }
    block_t* block = malloc(sizeof(block_t));
    block->version = version;
    memcpy(block->prev_block_hash, prev_block_hash, 32);
    memcpy(block->merkle_root, merkle_root, 32);
    block->timestamp = timestamp;
    block->target = target;
    block->nonce = nonce;
    block->num_transactions = num_transactions;
    block->transactions = transactions;
    *size = offset;
    return block;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    size_t size = file_size;
    block_t* block = block_parser(data, &size);

    if (block) {
        printf("Version: %u\n", block->version);
        printf("Previous Block Hash: ");
        for (int i = 0; i < 32; i++) {
            printf("%02x", block->prev_block_hash[i]);
        }
        printf("\n");
        printf("Merkle Root: ");
        for (int i = 0; i < 32; i++) {
            printf("%02x", block->merkle_root[i]);
        }
        printf("\n");
        printf("Timestamp: %u\n", block->timestamp);
        printf("Target: %u\n", block->target);
        printf("Nonce: %u\n", block->nonce);
        printf("Number of Transactions: %llu\n", block->num_transactions);
        for (int i = 0; i < block->num_transactions; i++) {
            transaction_t* transaction = &block->transactions[i];
            printf("Transaction Version: %u\n", transaction->version);
            printf("Transaction ID: ");
            for (int j = 0; j < 32; j++) {
                printf("%02x", transaction->tx_id[j]);
            }
            printf("\n");
            printf("Number of Inputs: %llu\n", transaction->num_inputs);
            for (int j = 0; j < transaction->num_inputs; j++) {
                printf("Input %d:\n", j);
                printf("Previous Transaction Hash: ");
                for (int k = 0; k < 32; k++) {
                    printf("%02x", transaction->inputs[j].prev_tx_hash[k]);
                }
                printf("\n");
                printf("Previous Transaction Index: %u\n", transaction->inputs[j].prev_tx_index);
                printf("Script Length: %llu\n", transaction->inputs[j].script_length);
                printf("Script: ");
                for (int k = 0; k < transaction->inputs[j].script_length; k++) {
                    printf("%02x", transaction->inputs[j].script[k]);
                }
                printf("\n");
                printf("Sequence Number: %u\n", transaction->inputs[j].sequence_number);
            }
            printf("Number of Outputs: %llu\n", transaction->num_outputs);
            for (int j = 0; j < transaction->num_outputs; j++) {
                printf("Output %d:\n", j);
                printf("Value: %llu\n", transaction->outputs[j].value);
                printf("Script Length: %llu\n", transaction->outputs[j].script_length);
                printf("Script: ");
                for (int k = 0; k < transaction->outputs[j].script_length; k++) {
                    printf("%02x", transaction->outputs[j].script[k]);
                }
                printf("\n");
            }
            printf("Lock Time: %u\n", transaction->lock_time);
        }
    } else {
        printf("Error parsing file\n");
    }

    free(data);
    return 0;
}