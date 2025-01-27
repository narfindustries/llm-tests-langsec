#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// Define the structure for a Bitcoin transaction
typedef struct {
    uint32_t version;
    uint8_t num_inputs;
    uint8_t* inputs;
    uint8_t num_outputs;
    uint8_t* outputs;
    uint32_t locktime;
} bitcoin_transaction_t;

// Define the structure for a transaction input
typedef struct {
    uint8_t* txid;
    uint32_t vout;
    uint8_t* script_sig;
    uint32_t sequence;
} transaction_input_t;

// Define the structure for a transaction output
typedef struct {
    uint64_t value;
    uint8_t* script_pub_key;
} transaction_output_t;

// Define a function to parse a Bitcoin transaction from a byte stream
bitcoin_transaction_t* parse_bitcoin_transaction(uint8_t* data, uint32_t length) {
    bitcoin_transaction_t* transaction = (bitcoin_transaction_t*)malloc(sizeof(bitcoin_transaction_t));

    // Read the version
    transaction->version = *(uint32_t*)data;
    data += 4;

    // Read the number of inputs
    transaction->num_inputs = *(uint8_t*)data;
    data += 1;

    // Read the inputs
    transaction->inputs = (uint8_t*)malloc(transaction->num_inputs * sizeof(transaction_input_t));
    for (uint8_t i = 0; i < transaction->num_inputs; i++) {
        transaction_input_t* input = (transaction_input_t*)(transaction->inputs + i * sizeof(transaction_input_t));

        // Read the txid
        input->txid = (uint8_t*)malloc(32);
        memcpy(input->txid, data, 32);
        data += 32;

        // Read the vout
        input->vout = *(uint32_t*)data;
        data += 4;

        // Read the script sig
        uint8_t script_sig_length = *(uint8_t*)data;
        data += 1;
        input->script_sig = (uint8_t*)malloc(script_sig_length);
        memcpy(input->script_sig, data, script_sig_length);
        data += script_sig_length;

        // Read the sequence
        input->sequence = *(uint32_t*)data;
        data += 4;
    }

    // Read the number of outputs
    transaction->num_outputs = *(uint8_t*)data;
    data += 1;

    // Read the outputs
    transaction->outputs = (uint8_t*)malloc(transaction->num_outputs * sizeof(transaction_output_t));
    for (uint8_t i = 0; i < transaction->num_outputs; i++) {
        transaction_output_t* output = (transaction_output_t*)(transaction->outputs + i * sizeof(transaction_output_t));

        // Read the value
        output->value = *(uint64_t*)data;
        data += 8;

        // Read the script pub key
        uint8_t script_pub_key_length = *(uint8_t*)data;
        data += 1;
        output->script_pub_key = (uint8_t*)malloc(script_pub_key_length);
        memcpy(output->script_pub_key, data, script_pub_key_length);
        data += script_pub_key_length;
    }

    // Read the locktime
    transaction->locktime = *(uint32_t*)data;
    data += 4;

    return transaction;
}

int main() {
    // Example usage:
    uint8_t data[] = {
        // Version
        0x01, 0x00, 0x00, 0x00,

        // Number of inputs
        0x01,

        // Input 1
        // Txid
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // Vout
        0x00, 0x00, 0x00, 0x00,
        // Script sig
        0x08,  // Length
        0x30, 0x44, 0x02, 0x20, 0x6b, 0x5f, 0x31, 0x30,
        // Sequence
        0xff, 0xff, 0xff, 0xff,

        // Number of outputs
        0x01,

        // Output 1
        // Value
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // Script pub key
        0x19,  // Length
        0x76, 0xa9, 0x14, 0x19, 0x94, 0x6c, 0x96, 0x53, 0x62, 0x5f, 0x32, 0x5f, 0x31, 0x30, 0x88, 0xac,

        // Locktime
        0x00, 0x00, 0x00, 0x00
    };

    bitcoin_transaction_t* transaction = parse_bitcoin_transaction(data, sizeof(data));

    // Print the transaction details
    printf("Version: %u\n", transaction->version);
    printf("Number of inputs: %u\n", transaction->num_inputs);
    for (uint8_t i = 0; i < transaction->num_inputs; i++) {
        transaction_input_t* input = (transaction_input_t*)(transaction->inputs + i * sizeof(transaction_input_t));
        printf("Input %u Txid: ", i + 1);
        for (uint8_t j = 0; j < 32; j++) {
            printf("%02x", input->txid[j]);
        }
        printf("\n");
    }
    printf("Number of outputs: %u\n", transaction->num_outputs);
    for (uint8_t i = 0; i < transaction->num_outputs; i++) {
        transaction_output_t* output = (transaction_output_t*)(transaction->outputs + i * sizeof(transaction_output_t));
        printf("Output %u Value: %llu\n", i + 1, output->value);
    }
    printf("Locktime: %u\n", transaction->locktime);

    return 0;
}