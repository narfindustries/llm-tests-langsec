#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser for var_int
HParser* var_int_parser() {
    return h_choice(
        h_sequence(h_ch(0xFD), h_uint16(), NULL),
        h_sequence(h_ch(0xFE), h_uint32(), NULL),
        h_sequence(h_ch(0xFF), h_uint64(), NULL),
        h_uint8(),
        NULL
    );
}

// Parser for transaction input
HParser* tx_input_parser() {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),  // Previous Transaction Hash (32 bytes)
        h_uint32(),                 // Previous Output Index (4 bytes)
        var_int_parser(),           // Script Length
        h_many(h_uint8()),         // ScriptSig
        h_uint32(),                // Sequence Number
        NULL
    );
}

// Parser for transaction output
HParser* tx_output_parser() {
    return h_sequence(
        h_uint64(),                // Value (8 bytes)
        var_int_parser(),          // Script Length
        h_many(h_uint8()),        // ScriptPubKey
        NULL
    );
}

// Parser for complete transaction
HParser* transaction_parser() {
    return h_sequence(
        h_uint32(),               // Version (4 bytes)
        var_int_parser(),         // Input Counter
        h_many(tx_input_parser()), // Transaction Inputs
        var_int_parser(),         // Output Counter
        h_many(tx_output_parser()), // Transaction Outputs
        h_uint32(),               // LockTime (4 bytes)
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <bitcoin_transaction_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file content
    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }
    
    size_t bytes_read = fread(data, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        fprintf(stderr, "Failed to read entire file\n");
        free(data);
        return 1;
    }

    // Initialize parser
    HParser *parser = transaction_parser();
    
    // Parse the data
    HParseResult *result = h_parse(parser, data, file_size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse transaction\n");
        free(data);
        return 1;
    }

    // Success
    printf("Successfully parsed Bitcoin transaction\n");
    
    // Clean up
    h_parse_result_free(result);
    free(data);
    
    return 0;
}