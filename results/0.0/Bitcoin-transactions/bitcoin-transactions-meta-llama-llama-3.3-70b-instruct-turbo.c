#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for a Bitcoin transaction
typedef struct {
    uint32_t version;
    uint8_t num_inputs;
    uint8_t inputs[100]; // assuming max 100 inputs
    uint8_t num_outputs;
    uint8_t outputs[100]; // assuming max 100 outputs
    uint32_t locktime;
} bitcoin_transaction_t;

// Define the structure for a Bitcoin transaction input
typedef struct {
    uint8_t prev_tx_hash[32];
    uint32_t prev_tx_out_index;
    uint8_t script_length;
    uint8_t script[100]; // assuming max 100 bytes script
    uint32_t sequence;
} bitcoin_transaction_input_t;

// Define the structure for a Bitcoin transaction output
typedef struct {
    uint64_t value;
    uint8_t script_length;
    uint8_t script[100]; // assuming max 100 bytes script
} bitcoin_transaction_output_t;

// Function to parse a Bitcoin transaction
void parse_bitcoin_transaction(uint8_t* data, uint32_t length) {
    bitcoin_transaction_t transaction;
    uint8_t* ptr = data;

    // Parse transaction version
    transaction.version = *(uint32_t*)ptr;
    ptr += 4;

    // Parse number of inputs
    transaction.num_inputs = *ptr;
    ptr++;

    // Parse inputs
    for (int i = 0; i < transaction.num_inputs; i++) {
        bitcoin_transaction_input_t input;
        // Parse previous transaction hash
        memcpy(input.prev_tx_hash, ptr, 32);
        ptr += 32;

        // Parse previous transaction output index
        input.prev_tx_out_index = *(uint32_t*)ptr;
        ptr += 4;

        // Parse script length
        input.script_length = *ptr;
        ptr++;

        // Parse script
        memcpy(input.script, ptr, input.script_length);
        ptr += input.script_length;

        // Parse sequence
        input.sequence = *(uint32_t*)ptr;
        ptr += 4;

        // Store input in transaction
        transaction.inputs[i] = (uint8_t)&input;
    }

    // Parse number of outputs
    transaction.num_outputs = *ptr;
    ptr++;

    // Parse outputs
    for (int i = 0; i < transaction.num_outputs; i++) {
        bitcoin_transaction_output_t output;
        // Parse value
        output.value = *(uint64_t*)ptr;
        ptr += 8;

        // Parse script length
        output.script_length = *ptr;
        ptr++;

        // Parse script
        memcpy(output.script, ptr, output.script_length);
        ptr += output.script_length;

        // Store output in transaction
        transaction.outputs[i] = (uint8_t)&output;
    }

    // Parse locktime
    transaction.locktime = *(uint32_t*)ptr;

    // Print transaction details
    printf("Transaction version: %u\n", transaction.version);
    printf("Number of inputs: %u\n", transaction.num_inputs);
    printf("Number of outputs: %u\n", transaction.num_outputs);
    printf("Locktime: %u\n", transaction.locktime);

    // Print input details
    for (int i = 0; i < transaction.num_inputs; i++) {
        bitcoin_transaction_input_t* input = (bitcoin_transaction_input_t*)transaction.inputs[i];
        printf("Input %u:\n", i);
        printf("Previous transaction hash: ");
        for (int j = 0; j < 32; j++) {
            printf("%02x", input->prev_tx_hash[j]);
        }
        printf("\n");
        printf("Previous transaction output index: %u\n", input->prev_tx_out_index);
        printf("Script length: %u\n", input->script_length);
        printf("Script: ");
        for (int j = 0; j < input->script_length; j++) {
            printf("%02x", input->script[j]);
        }
        printf("\n");
        printf("Sequence: %u\n", input->sequence);
    }

    // Print output details
    for (int i = 0; i < transaction.num_outputs; i++) {
        bitcoin_transaction_output_t* output = (bitcoin_transaction_output_t*)transaction.outputs[i];
        printf("Output %u:\n", i);
        printf("Value: %llu\n", output->value);
        printf("Script length: %u\n", output->script_length);
        printf("Script: ");
        for (int j = 0; j < output->script_length; j++) {
            printf("%02x", output->script[j]);
        }
        printf("\n");
    }
}

int main() {
    // Example Bitcoin transaction data
    uint8_t data[] = {
        0x01, 0x00, 0x00, 0x00, // version
        0x01, // num_inputs
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // prev_tx_hash
        0x00, 0x00, 0x00, 0x00, // prev_tx_out_index
        0x08, // script_length
        0x76, 0xa9, 0x14, 0x1a, 0xa0, 0xd6, 0x47, 0x8a, // script
        0xff, 0xff, 0xff, 0xff, // sequence
        0x01, // num_outputs
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // value
        0x19, // script_length
        0x76, 0xa9, 0x14, 0x1a, 0xa0, 0xd6, 0x47, 0x8a, 0x88, 0xac, // script
        0x00, 0x00, 0x00, 0x00 // locktime
    };

    parse_bitcoin_transaction(data, sizeof(data));

    return 0;
}