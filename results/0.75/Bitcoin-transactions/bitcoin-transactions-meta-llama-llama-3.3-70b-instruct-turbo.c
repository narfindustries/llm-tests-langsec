#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint8_t* previous_output_hash;
    uint32_t previous_output_index;
    uint64_t script_length;
    uint8_t* script_signature;
    uint32_t sequence_number;
} bitcoin_tx_in_t;

typedef struct {
    uint64_t value;
    uint64_t script_length;
    uint8_t* script_public_key;
} bitcoin_tx_out_t;

typedef struct {
    uint32_t version;
    uint64_t tx_in_count;
    bitcoin_tx_in_t* tx_in;
    uint64_t tx_out_count;
    bitcoin_tx_out_t* tx_out;
} bitcoin_transaction_t;

typedef struct {
    uint32_t block_version;
    uint8_t previous_block_hash[32];
    uint8_t merkle_root[32];
    uint32_t timestamp;
    uint32_t target;
    uint32_t nonce;
    uint64_t transaction_count;
    bitcoin_transaction_t* transactions;
} bitcoin_block_t;

void* bitcoin_varint(void* p) {
    void* parser = hammer_uint64(p);
    if (hammer_is_error(parser)) return parser;
    uint64_t value = *(uint64_t*)hammer_get_bytes(parser);
    uint8_t* bytes = malloc(9);
    uint8_t length = 0;
    do {
        bytes[length++] = (value & 0x7F) | (length == 0 ? 0 : 0x80);
        value >>= 7;
    } while (value);
    hammer_set_bytes(parser, bytes, length);
    free(bytes);
    return parser;
}

void* bitcoin_script_length(void* p) {
    return bitcoin_varint(p);
}

void* bitcoin_script_signature(void* p) {
    void* length_parser = bitcoin_script_length(p);
    if (hammer_is_error(length_parser)) return length_parser;
    uint64_t length = *(uint64_t*)hammer_get_bytes(length_parser);
    void* bytes_parser = hammer_bytes(p, length);
    if (hammer_is_error(bytes_parser)) return bytes_parser;
    return bytes_parser;
}

void* bitcoin_script_public_key(void* p) {
    return bitcoin_script_signature(p);
}

void* bitcoin_tx_in(void* p) {
    void* previous_output_hash_parser = hammer_bytes(p, 32);
    if (hammer_is_error(previous_output_hash_parser)) return previous_output_hash_parser;
    uint8_t* previous_output_hash = hammer_get_bytes(previous_output_hash_parser);
    void* previous_output_index_parser = hammer_uint32(p);
    if (hammer_is_error(previous_output_index_parser)) return previous_output_index_parser;
    uint32_t previous_output_index = *(uint32_t*)hammer_get_bytes(previous_output_index_parser);
    void* script_length_parser = bitcoin_script_length(p);
    if (hammer_is_error(script_length_parser)) return script_length_parser;
    uint64_t script_length = *(uint64_t*)hammer_get_bytes(script_length_parser);
    void* script_signature_parser = hammer_bytes(p, script_length);
    if (hammer_is_error(script_signature_parser)) return script_signature_parser;
    uint8_t* script_signature = hammer_get_bytes(script_signature_parser);
    void* sequence_number_parser = hammer_uint32(p);
    if (hammer_is_error(sequence_number_parser)) return sequence_number_parser;
    uint32_t sequence_number = *(uint32_t*)hammer_get_bytes(sequence_number_parser);
    bitcoin_tx_in_t* tx_in = malloc(sizeof(bitcoin_tx_in_t));
    tx_in->previous_output_hash = previous_output_hash;
    tx_in->previous_output_index = previous_output_index;
    tx_in->script_signature = script_signature;
    tx_in->script_length = script_length;
    tx_in->sequence_number = sequence_number;
    hammer_set_bytes(p, NULL, 0);
    return hammer_success(p, tx_in);
}

void* bitcoin_tx_out(void* p) {
    void* value_parser = hammer_uint64(p);
    if (hammer_is_error(value_parser)) return value_parser;
    uint64_t value = *(uint64_t*)hammer_get_bytes(value_parser);
    void* script_length_parser = bitcoin_script_length(p);
    if (hammer_is_error(script_length_parser)) return script_length_parser;
    uint64_t script_length = *(uint64_t*)hammer_get_bytes(script_length_parser);
    void* script_public_key_parser = hammer_bytes(p, script_length);
    if (hammer_is_error(script_public_key_parser)) return script_public_key_parser;
    uint8_t* script_public_key = hammer_get_bytes(script_public_key_parser);
    bitcoin_tx_out_t* tx_out = malloc(sizeof(bitcoin_tx_out_t));
    tx_out->value = value;
    tx_out->script_public_key = script_public_key;
    tx_out->script_length = script_length;
    hammer_set_bytes(p, NULL, 0);
    return hammer_success(p, tx_out);
}

void* bitcoin_transaction(void* p) {
    void* version_parser = hammer_uint32(p);
    if (hammer_is_error(version_parser)) return version_parser;
    uint32_t version = *(uint32_t*)hammer_get_bytes(version_parser);
    void* tx_in_count_parser = bitcoin_varint(p);
    if (hammer_is_error(tx_in_count_parser)) return tx_in_count_parser;
    uint64_t tx_in_count = *(uint64_t*)hammer_get_bytes(tx_in_count_parser);
    bitcoin_tx_in_t* tx_in = malloc(tx_in_count * sizeof(bitcoin_tx_in_t));
    for (uint64_t i = 0; i < tx_in_count; i++) {
        void* tx_in_parser = bitcoin_tx_in(p);
        if (hammer_is_error(tx_in_parser)) return tx_in_parser;
        tx_in[i] = *(bitcoin_tx_in_t*)hammer_get_bytes(tx_in_parser);
    }
    void* tx_out_count_parser = bitcoin_varint(p);
    if (hammer_is_error(tx_out_count_parser)) return tx_out_count_parser;
    uint64_t tx_out_count = *(uint64_t*)hammer_get_bytes(tx_out_count_parser);
    bitcoin_tx_out_t* tx_out = malloc(tx_out_count * sizeof(bitcoin_tx_out_t));
    for (uint64_t i = 0; i < tx_out_count; i++) {
        void* tx_out_parser = bitcoin_tx_out(p);
        if (hammer_is_error(tx_out_parser)) return tx_out_parser;
        tx_out[i] = *(bitcoin_tx_out_t*)hammer_get_bytes(tx_out_parser);
    }
    void* lock_time_parser = hammer_uint32(p);
    if (hammer_is_error(lock_time_parser)) return lock_time_parser;
    uint32_t lock_time = *(uint32_t*)hammer_get_bytes(lock_time_parser);
    bitcoin_transaction_t* transaction = malloc(sizeof(bitcoin_transaction_t));
    transaction->version = version;
    transaction->tx_in_count = tx_in_count;
    transaction->tx_in = tx_in;
    transaction->tx_out_count = tx_out_count;
    transaction->tx_out = tx_out;
    hammer_set_bytes(p, NULL, 0);
    return hammer_success(p, transaction);
}

void* bitcoin_block(void* p) {
    void* block_version_parser = hammer_uint32(p);
    if (hammer_is_error(block_version_parser)) return block_version_parser;
    uint32_t block_version = *(uint32_t*)hammer_get_bytes(block_version_parser);
    void* previous_block_hash_parser = hammer_bytes(p, 32);
    if (hammer_is_error(previous_block_hash_parser)) return previous_block_hash_parser;
    uint8_t* previous_block_hash = hammer_get_bytes(previous_block_hash_parser);
    void* merkle_root_parser = hammer_bytes(p, 32);
    if (hammer_is_error(merkle_root_parser)) return merkle_root_parser;
    uint8_t* merkle_root = hammer_get_bytes(merkle_root_parser);
    void* timestamp_parser = hammer_uint32(p);
    if (hammer_is_error(timestamp_parser)) return timestamp_parser;
    uint32_t timestamp = *(uint32_t*)hammer_get_bytes(timestamp_parser);
    void* target_parser = hammer_uint32(p);
    if (hammer_is_error(target_parser)) return target_parser;
    uint32_t target = *(uint32_t*)hammer_get_bytes(target_parser);
    void* nonce_parser = hammer_uint32(p);
    if (hammer_is_error(nonce_parser)) return nonce_parser;
    uint32_t nonce = *(uint32_t*)hammer_get_bytes(nonce_parser);
    void* transaction_count_parser = bitcoin_varint(p);
    if (hammer_is_error(transaction_count_parser)) return transaction_count_parser;
    uint64_t transaction_count = *(uint64_t*)hammer_get_bytes(transaction_count_parser);
    bitcoin_transaction_t* transactions = malloc(transaction_count * sizeof(bitcoin_transaction_t));
    for (uint64_t i = 0; i < transaction_count; i++) {
        void* transaction_parser = bitcoin_transaction(p);
        if (hammer_is_error(transaction_parser)) return transaction_parser;
        transactions[i] = *(bitcoin_transaction_t*)hammer_get_bytes(transaction_parser);
    }
    bitcoin_block_t* block = malloc(sizeof(bitcoin_block_t));
    block->block_version = block_version;
    memcpy(block->previous_block_hash, previous_block_hash, 32);
    memcpy(block->merkle_root, merkle_root, 32);
    block->timestamp = timestamp;
    block->target = target;
    block->nonce = nonce;
    block->transaction_count = transaction_count;
    block->transactions = transactions;
    hammer_set_bytes(p, NULL, 0);
    return hammer_success(p, block);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }
    FILE* input_file = fopen(argv[1], "rb");
    if (!input_file) {
        printf("Error opening input file\n");
        return 1;
    }
    fseek(input_file, 0, SEEK_END);
    long input_size = ftell(input_file);
    rewind(input_file);
    uint8_t* input_bytes = malloc(input_size);
    fread(input_bytes, 1, input_size, input_file);
    fclose(input_file);
    void* parser = hammer_bytes(input_bytes, input_size);
    void* block_parser = bitcoin_block(parser);
    if (hammer_is_error(block_parser)) {
        printf("Error parsing block\n");
        return 1;
    }
    bitcoin_block_t* block = (bitcoin_block_t*)hammer_get_bytes(block_parser);
    printf("Block version: %u\n", block->block_version);
    printf("Previous block hash: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", block->previous_block_hash[i]);
    }
    printf("\n");
    printf("Merkle root: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", block->merkle_root[i]);
    }
    printf("\n");
    printf("Timestamp: %u\n", block->timestamp);
    printf("Target: %u\n", block->target);
    printf("Nonce: %u\n", block->nonce);
    printf("Transaction count: %llu\n", block->transaction_count);
    for (uint64_t i = 0; i < block->transaction_count; i++) {
        bitcoin_transaction_t* transaction = &block->transactions[i];
        printf("Transaction version: %u\n", transaction->version);
        printf("Transaction input count: %llu\n", transaction->tx_in_count);
        for (uint64_t j = 0; j < transaction->tx_in_count; j++) {
            bitcoin_tx_in_t* tx_in = &transaction->tx_in[j];
            printf("Transaction input previous output hash: ");
            for (int k = 0; k < 32; k++) {
                printf("%02x", tx_in->previous_output_hash[k]);
            }
            printf("\n");
            printf("Transaction input previous output index: %u\n", tx_in->previous_output_index);
            printf("Transaction input script signature: ");
            for (uint64_t k = 0; k < tx_in->script_length; k++) {
                printf("%02x", tx_in->script_signature[k]);
            }
            printf("\n");
            printf("Transaction input sequence number: %u\n", tx_in->sequence_number);
        }
        printf("Transaction output count: %llu\n", transaction->tx_out_count);
        for (uint64_t j = 0; j < transaction->tx_out_count; j++) {
            bitcoin_tx_out_t* tx_out = &transaction->tx_out[j];
            printf("Transaction output value: %llu\n", tx_out->value);
            printf("Transaction output script public key: ");
            for (uint64_t k = 0; k < tx_out->script_length; k++) {
                printf("%02x", tx_out->script_public_key[k]);
            }
            printf("\n");
        }
    }
    return 0;
}