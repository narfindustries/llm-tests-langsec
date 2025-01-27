#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Define structs for Bitcoin transaction
typedef struct {
    uint32_t version;
    uint8_t num_inputs;
    uint8_t num_outputs;
} transaction_header_t;

typedef struct {
    uint8_t script_length;
    uint8_t script_data[65]; // 65 is max length for compressed pub key
    uint32_t output_value;
    uint8_t num_witness;
} output_t;

typedef struct {
    uint8_t script_length;
    uint8_t script_data[65]; // 65 is max length for compressed pub key
    uint32_t previous_output_index;
    uint32_t previous_output_height;
    uint32_t sequence;
} input_t;

typedef struct {
    transaction_header_t header;
    input_t inputs[1]; // assume single input
    output_t outputs[1]; // assume single output
    uint32_t lock_time;
} bitcoin_transaction_t;

// Define functions to read/write Bitcoin transaction
void read_transaction(bitcoin_transaction_t* transaction, FILE* file) {
    fread(&transaction->header, sizeof(transaction_header_t), 1, file);
    fread(&transaction->inputs, sizeof(input_t), transaction->header.num_inputs, file);
    fread(&transaction->outputs, sizeof(output_t), transaction->header.num_outputs, file);
    fread(&transaction->lock_time, sizeof(uint32_t), 1, file);
}

void write_transaction(bitcoin_transaction_t* transaction, FILE* file) {
    fwrite(&transaction->header, sizeof(transaction_header_t), 1, file);
    fwrite(&transaction->inputs, sizeof(input_t), transaction->header.num_inputs, file);
    fwrite(&transaction->outputs, sizeof(output_t), transaction->header.num_outputs, file);
    fwrite(&transaction->lock_time, sizeof(uint32_t), 1, file);
}

int main() {
    // Example usage
    bitcoin_transaction_t transaction;
    FILE* file = fopen("bitcoin_transaction.bin", "rb");
    if (file == NULL) {
        perror("Failed to open file");
        return 1;
    }

    read_transaction(&transaction, file);
    fclose(file);

    // Print transaction details
    printf("Transaction Version: %u\n", transaction.header.version);
    printf("Number of Inputs: %u\n", transaction.header.num_inputs);
    printf("Number of Outputs: %u\n", transaction.header.num_outputs);
    printf("Lock Time: %u\n", transaction.lock_time);

    // Modify transaction (if needed)
    // transaction.header.version = 2;

    // Write modified transaction back to file
    file = fopen("modified_bitcoin_transaction.bin", "wb");
    if (file == NULL) {
        perror("Failed to open file");
        return 1;
    }

    write_transaction(&transaction, file);
    fclose(file);

    return 0;
}