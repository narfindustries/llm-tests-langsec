#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
HParser* create_bitcoin_transaction_parser(void);
HParser* create_var_int_parser(void);
HParser* create_transaction_input_parser(void);
HParser* create_transaction_output_parser(void);

// Variable length integer parser
HParser* create_var_int_parser(void) {
    return h_choice(h_sequence(h_uint8(), h_nothing_p(), NULL),
                   h_sequence(h_ch(0xFD), h_uint16(), NULL),
                   h_sequence(h_ch(0xFE), h_uint32(), NULL),
                   h_sequence(h_ch(0xFF), h_uint64(), NULL),
                   NULL);
}

// Transaction input parser
HParser* create_transaction_input_parser(void) {
    return h_sequence(
        h_repeat_n(h_uint8(), 32), // Previous transaction hash
        h_uint32(),                // Previous output index
        create_var_int_parser(),   // Script length
        h_length_value(            // ScriptSig
            create_var_int_parser(),
            h_many1(h_uint8())),
        h_uint32(),                // Sequence
        NULL);
}

// Transaction output parser
HParser* create_transaction_output_parser(void) {
    return h_sequence(
        h_uint64(),                // Value in satoshis
        create_var_int_parser(),   // Script length
        h_length_value(            // ScriptPubKey
            create_var_int_parser(),
            h_many1(h_uint8())),
        NULL);
}

// Main transaction parser
HParser* create_bitcoin_transaction_parser(void) {
    return h_sequence(
        h_uint32(),                // Version
        create_var_int_parser(),   // Input counter
        h_length_value(            // Transaction inputs
            create_var_int_parser(),
            h_many1(create_transaction_input_parser())),
        create_var_int_parser(),   // Output counter
        h_length_value(            // Transaction outputs
            create_var_int_parser(),
            h_many1(create_transaction_output_parser())),
        h_uint32(),                // Locktime
        NULL);
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

    if (fread(input, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        fclose(file);
        free(input);
        return 1;
    }
    fclose(file);

    // Initialize parser
    HParser *transaction_parser = create_bitcoin_transaction_parser();
    
    // Parse input
    HParseResult *result = h_parse(transaction_parser, input, file_size);
    
    if (result) {
        printf("Successfully parsed Bitcoin transaction\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse Bitcoin transaction\n");
    }

    free(input);
    return 0;
}