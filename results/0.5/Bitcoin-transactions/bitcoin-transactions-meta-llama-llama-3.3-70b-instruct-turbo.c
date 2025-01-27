#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for a Bitcoin transaction
typedef struct {
    uint32_t version;
    uint8_t num_inputs;
    uint8_t inputs[];
    uint8_t num_outputs;
    uint8_t outputs[];
    uint32_t lock_time;
} __attribute__((packed)) bitcoin_transaction_t;

// Define the structure for a transaction input
typedef struct {
    uint8_t prev_tx_hash[32];
    uint32_t prev_tx_out_index;
    uint8_t script_length;
    uint8_t script[];
    uint32_t sequence;
} __attribute__((packed)) transaction_input_t;

// Define the structure for a transaction output
typedef struct {
    uint64_t value;
    uint8_t script_length;
    uint8_t script[];
} __attribute__((packed)) transaction_output_t;

// Define the function to parse a Bitcoin transaction
void parse_bitcoin_transaction(uint8_t* data, size_t size) {
    bitcoin_transaction_t* transaction = (bitcoin_transaction_t*) data;

    // Check if the transaction is valid
    if (size < sizeof(uint32_t) + sizeof(uint8_t) + sizeof(uint8_t) + sizeof(uint32_t)) {
        printf("Invalid transaction size\n");
        return;
    }

    // Print the transaction version
    printf("Transaction version: %u\n", transaction->version);

    // Parse the transaction inputs
    uint8_t* input_ptr = (uint8_t*) &transaction->inputs;
    for (int i = 0; i < transaction->num_inputs; i++) {
        transaction_input_t* input = (transaction_input_t*) input_ptr;

        // Check if the input is valid
        if (size < (input_ptr - data) + sizeof(uint8_t[32]) + sizeof(uint32_t) + sizeof(uint8_t) + sizeof(uint32_t)) {
            printf("Invalid input size\n");
            return;
        }

        // Print the previous transaction hash
        printf("Previous transaction hash: ");
        for (int j = 0; j < 32; j++) {
            printf("%02x", input->prev_tx_hash[j]);
        }
        printf("\n");

        // Print the previous transaction output index
        printf("Previous transaction output index: %u\n", input->prev_tx_out_index);

        // Parse the script
        uint8_t* script_ptr = (uint8_t*) &input->script;
        printf("Script length: %u\n", input->script_length);
        printf("Script: ");
        for (int j = 0; j < input->script_length; j++) {
            printf("%02x", script_ptr[j]);
        }
        printf("\n");

        // Print the sequence
        printf("Sequence: %u\n", input->sequence);

        // Move to the next input
        input_ptr += sizeof(uint8_t[32]) + sizeof(uint32_t) + sizeof(uint8_t) + input->script_length + sizeof(uint32_t);
    }

    // Parse the transaction outputs
    uint8_t* output_ptr = (uint8_t*) &transaction->outputs;
    for (int i = 0; i < transaction->num_outputs; i++) {
        transaction_output_t* output = (transaction_output_t*) output_ptr;

        // Check if the output is valid
        if (size < (output_ptr - data) + sizeof(uint64_t) + sizeof(uint8_t) + sizeof(uint32_t)) {
            printf("Invalid output size\n");
            return;
        }

        // Print the value
        printf("Value: %llu\n", output->value);

        // Parse the script
        uint8_t* script_ptr = (uint8_t*) &output->script;
        printf("Script length: %u\n", output->script_length);
        printf("Script: ");
        for (int j = 0; j < output->script_length; j++) {
            printf("%02x", script_ptr[j]);
        }
        printf("\n");

        // Move to the next output
        output_ptr += sizeof(uint64_t) + sizeof(uint8_t) + output->script_length;
    }

    // Print the lock time
    printf("Lock time: %u\n", transaction->lock_time);
}

int main() {
    // Example usage
    uint8_t data[] = {
        // Transaction version
        0x01, 0x00, 0x00, 0x00,

        // Number of inputs
        0x01,

        // Input 1
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

        // Number of outputs
        0x01,

        // Output 1
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

        // Lock time
        0x00, 0x00, 0x00, 0x00
    };

    parse_bitcoin_transaction(data, sizeof(data));

    return 0;
}