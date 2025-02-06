#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define byte value parsers to fix implicit declaration issues
HParser *byte_value(uint8_t value) {
    return h_uint8_val(value);
}

// Define parsers for various Bitcoin transaction components
HParser *var_int_parser(void) {
    return h_choice(
        h_uint8(),
        h_sequence(byte_value(0xfd), h_uint16(), NULL),
        h_sequence(byte_value(0xfe), h_uint32(), NULL),
        h_sequence(byte_value(0xff), h_uint64(), NULL),
        NULL
    );
}

HParser *script_parser(void) {
    return h_bind(
        var_int_parser(),
        [](hparse_result *res, void *arg) {
            size_t len = (size_t)h_parse_result_value(res);
            return h_repeat_n(h_uint8(), len);
        },
        NULL
    );
}

HParser *input_parser(void) {
    return h_sequence(
        h_repeat_n(h_uint8(), 32),       // Previous Transaction Hash
        h_uint32(),                      // Previous Transaction Index
        script_parser(),                 // Unlocking Script
        h_uint32(),                      // Sequence Number
        NULL
    );
}

HParser *output_parser(void) {
    return h_sequence(
        h_uint64(),                      // Value
        script_parser(),                 // Locking Script
        NULL
    );
}

HParser *transaction_parser(void) {
    return h_sequence(
        h_uint32(),                      // Version
        var_int_parser(),                // Input Count
        h_many1(input_parser()),         // Input List
        var_int_parser(),                // Output Count
        h_many1(output_parser()),        // Output List
        h_uint32(),                      // Lock Time
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);
    unsigned char *data = (unsigned char *)malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    if (fread(data, 1, length, file) != length) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return EXIT_FAILURE;
    }
    fclose(file);

    HParser *tx_parser = transaction_parser();
    HParseResult *result = h_parse(tx_parser, data, length);

    if (result) {
        printf("Transaction parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parse error!\n");
    }

    free(data);
    h_parser_free(tx_parser);
    return EXIT_SUCCESS;
}