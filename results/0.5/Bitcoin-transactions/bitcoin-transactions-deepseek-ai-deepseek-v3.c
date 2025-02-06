#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint8_t prevout_hash[32];
    uint32_t prevout_index;
    uint64_t script_sig_len;
    uint8_t *script_sig;
    uint32_t sequence;
} BitcoinInput;

typedef struct {
    int64_t value;
    uint64_t script_pub_key_len;
    uint8_t *script_pub_key;
} BitcoinOutput;

typedef struct {
    uint64_t stack_count;
    uint8_t **stack_items;
} BitcoinWitness;

typedef struct {
    uint32_t version;
    uint64_t input_count;
    BitcoinInput *inputs;
    uint64_t output_count;
    BitcoinOutput *outputs;
    uint32_t lock_time;
    uint64_t witness_count;
    BitcoinWitness *witnesses;
} BitcoinTransaction;

HParser *parse_version() {
    return h_int32();
}

HParser *parse_var_int() {
    return h_choice(h_uint8(), h_uint16(), h_uint32(), h_uint64(), NULL);
}

HParser *parse_uint256() {
    return h_repeat_n(h_uint8(), 32);
}

HParser *parse_script(uint64_t len) {
    return h_repeat_n(h_uint8(), len);
}

HParser *parse_transaction() {
    return h_sequence(
        h_bind(parse_version(), (HContinuation)&version),
        h_bind(parse_var_int(), (HContinuation)&input_count),
        h_repeat_n(h_sequence(
            h_bind(parse_uint256(), (HContinuation)&prevout_hash),
            h_bind(h_uint32(), (HContinuation)&prevout_index),
            h_bind(parse_var_int(), (HContinuation)&script_sig_len),
            h_bind(parse_script(script_sig_len), (HContinuation)&script_sig),
            h_bind(h_uint32(), (HContinuation)&sequence)
        ), input_count, (HContinuation)&inputs),
        h_bind(parse_var_int(), (HContinuation)&output_count),
        h_repeat_n(h_sequence(
            h_bind(h_int64(), (HContinuation)&value),
            h_bind(parse_var_int(), (HContinuation)&script_pub_key_len),
            h_bind(parse_script(script_pub_key_len), (HContinuation)&script_pub_key)
        ), output_count, (HContinuation)&outputs),
        h_bind(h_uint32(), (HContinuation)&lock_time),
        h_bind(parse_var_int(), (HContinuation)&witness_count),
        h_repeat_n(h_sequence(
            h_bind(parse_var_int(), (HContinuation)&stack_count),
            h_repeat_n(h_bind(parse_script(stack_count), (HContinuation)&stack_items), stack_count)
        ), witness_count, (HContinuation)&witnesses),
        h_end_p()
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *parsed = h_parse(parse_transaction(), buffer, file_size);
    if (!parsed) {
        fprintf(stderr, "Failed to parse transaction\n");
        free(buffer);
        return 1;
    }

    BitcoinTransaction *tx = (BitcoinTransaction*)parsed->ast->user_data;
    printf("Version: %u\n", tx->version);
    printf("Input Count: %lu\n", tx->input_count);
    for (uint64_t i = 0; i < tx->input_count; i++) {
        printf("Input %lu:\n", i);
        printf("  Prevout Hash: ");
        for (int j = 0; j < 32; j++) {
            printf("%02x", tx->inputs[i].prevout_hash[j]);
        }
        printf("\n");
        printf("  Prevout Index: %u\n", tx->inputs[i].prevout_index);
        printf("  ScriptSig Length: %lu\n", tx->inputs[i].script_sig_len);
        printf("  ScriptSig: ");
        for (uint64_t j = 0; j < tx->inputs[i].script_sig_len; j++) {
            printf("%02x", tx->inputs[i].script_sig[j]);
        }
        printf("\n");
        printf("  Sequence: %u\n", tx->inputs[i].sequence);
    }
    printf("Output Count: %lu\n", tx->output_count);
    for (uint64_t i = 0; i < tx->output_count; i++) {
        printf("Output %lu:\n", i);
        printf("  Value: %ld\n", tx->outputs[i].value);
        printf("  ScriptPubKey Length: %lu\n", tx->outputs[i].script_pub_key_len);
        printf("  ScriptPubKey: ");
        for (uint64_t j = 0; j < tx->outputs[i].script_pub_key_len; j++) {
            printf("%02x", tx->outputs[i].script_pub_key[j]);
        }
        printf("\n");
    }
    printf("Lock Time: %u\n", tx->lock_time);
    printf("Witness Count: %lu\n", tx->witness_count);
    for (uint64_t i = 0; i < tx->witness_count; i++) {
        printf("Witness %lu:\n", i);
        printf("  Stack Count: %lu\n", tx->witnesses[i].stack_count);
        for (uint64_t j = 0; j < tx->witnesses[i].stack_count; j++) {
            printf("  Stack Item %lu: ", j);
            for (uint64_t k = 0; k < tx->witnesses[i].stack_items[j][0]; k++) {
                printf("%02x", tx->witnesses[i].stack_items[j][k]);
            }
            printf("\n");
        }
    }

    free(buffer);
    h_free_parse_result(parsed);
    return 0;
}