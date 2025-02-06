#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
HParser* create_transaction_parser(void);
HParser* create_var_int_parser(void);
HParser* create_tx_in_parser(void);
HParser* create_tx_out_parser(void);
HParser* create_script_parser(void);

// Variable length integer parser
HParser* create_var_int_parser(void) {
    return h_choice(h_sequence(h_uint8(), h_nothing_p(), NULL),
                   h_sequence(h_ch(0xFD), h_uint16(), NULL),
                   h_sequence(h_ch(0xFE), h_uint32(), NULL),
                   h_sequence(h_ch(0xFF), h_uint64(), NULL),
                   NULL);
}

// Script parser
HParser* create_script_parser(void) {
    return h_sequence(
        create_var_int_parser(),  // Script length
        h_many1(h_uint8()),      // Script data
        NULL
    );
}

// Transaction Input parser
HParser* create_tx_in_parser(void) {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),  // Previous Transaction Hash
        h_uint32(),                 // Previous Transaction Index
        create_script_parser(),      // ScriptSig
        h_uint32(),                 // Sequence
        NULL
    );
}

// Transaction Output parser
HParser* create_tx_out_parser(void) {
    return h_sequence(
        h_uint64(),                 // Value in satoshis
        create_script_parser(),      // ScriptPubKey
        NULL
    );
}

// Witness data parser (for SegWit)
HParser* create_witness_parser(void) {
    return h_sequence(
        create_var_int_parser(),     // Witness counter
        h_many1(h_sequence(
            create_var_int_parser(), // Witness item length
            h_many1(h_uint8()),      // Witness item data
            NULL
        )),
        NULL
    );
}

// Main transaction parser
HParser* create_transaction_parser(void) {
    return h_sequence(
        h_uint32(),                 // Version
        h_optional(h_sequence(      // SegWit marker and flag
            h_ch(0x00),
            h_ch(0x01),
            NULL
        )),
        create_var_int_parser(),    // Input counter
        h_many1(create_tx_in_parser()),  // Inputs
        create_var_int_parser(),    // Output counter
        h_many1(create_tx_out_parser()), // Outputs
        h_optional(create_witness_parser()), // Witness data (if SegWit)
        h_uint32(),                 // Locktime
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
    uint8_t *input = malloc(file_size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }
    
    size_t bytes_read = fread(input, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        fprintf(stderr, "Failed to read entire file\n");
        free(input);
        return 1;
    }

    // Initialize parser
    HParser *transaction_parser = create_transaction_parser();
    
    // Parse input
    HParseResult *result = h_parse(transaction_parser, input, file_size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse transaction\n");
        free(input);
        return 1;
    }

    // Success
    printf("Successfully parsed Bitcoin transaction\n");
    
    // Cleanup
    h_parse_result_free(result);
    free(input);
    
    return 0;
}