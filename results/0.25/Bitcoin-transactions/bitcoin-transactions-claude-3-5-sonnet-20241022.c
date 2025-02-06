#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
HParser* init_bitcoin_transaction_parser(void);
HParser* init_var_int_parser(void);
HParser* init_transaction_input_parser(void);
HParser* init_transaction_output_parser(void);
HParser* init_witness_data_parser(void);

// Helper parser for variable integers
HParser* init_var_int_parser(void) {
    return h_choice(h_sequence(h_uint8(), h_nothing_p(), NULL),
                   h_sequence(h_ch(0xFD), h_uint16(), NULL),
                   h_sequence(h_ch(0xFE), h_uint32(), NULL),
                   h_sequence(h_ch(0xFF), h_uint64(), NULL),
                   NULL);
}

// Parser for transaction inputs
HParser* init_transaction_input_parser(void) {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),   // Previous Transaction Hash
        h_uint32(),                   // Previous Transaction Index
        init_var_int_parser(),        // ScriptSig Length
        h_length_value(
            h_left(init_var_int_parser(), h_nothing_p()),
            h_repeat_n(h_uint8(), 1)  // ScriptSig
        ),
        h_uint32(),                   // Sequence Number
        NULL
    );
}

// Parser for transaction outputs
HParser* init_transaction_output_parser(void) {
    return h_sequence(
        h_uint64(),                   // Value in satoshis
        init_var_int_parser(),        // ScriptPubKey Length
        h_length_value(
            h_left(init_var_int_parser(), h_nothing_p()),
            h_repeat_n(h_uint8(), 1)  // ScriptPubKey
        ),
        NULL
    );
}

// Parser for witness data
HParser* init_witness_data_parser(void) {
    return h_sequence(
        init_var_int_parser(),        // Witness Stack Items Count
        h_many(h_sequence(
            init_var_int_parser(),    // Item Size
            h_length_value(
                h_left(init_var_int_parser(), h_nothing_p()),
                h_repeat_n(h_uint8(), 1) // Item Data
            ),
            NULL
        )),
        NULL
    );
}

// Main transaction parser
HParser* init_bitcoin_transaction_parser(void) {
    return h_sequence(
        h_uint32(),                   // Version
        h_optional(h_sequence(        // SegWit marker and flag (optional)
            h_ch(0x00),
            h_ch(0x01),
            NULL
        )),
        init_var_int_parser(),        // Input Counter
        h_many(init_transaction_input_parser()),  // Inputs
        init_var_int_parser(),        // Output Counter
        h_many(init_transaction_output_parser()), // Outputs
        h_optional(h_many(init_witness_data_parser())), // Witness Data (optional)
        h_uint32(),                   // Locktime
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <bitcoin_transaction_file>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }
    fclose(f);

    // Initialize parser
    HParser *bitcoin_transaction_parser = init_bitcoin_transaction_parser();
    
    // Parse input
    HParseResult *result = h_parse(bitcoin_transaction_parser, input, size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse Bitcoin transaction\n");
        free(input);
        return 1;
    }

    // Here you would typically process the parse tree in result->ast
    // For now, just indicate success
    printf("Successfully parsed Bitcoin transaction\n");

    // Cleanup
    h_parse_result_free(result);
    free(input);
    return 0;
}