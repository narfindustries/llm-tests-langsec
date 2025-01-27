#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// NTP v4 Parser using Hammer

// Helper parsers for common fields
HParser* li_vn_mode_parser() {
    return h_bits(8, false);
}

HParser* stratum_parser() {
    return h_bits(8, false);
}

HParser* poll_parser() {
    return h_bits(8, false);
}

HParser* precision_parser() {
    return h_bits(8, false);
}

HParser* root_delay_parser() {
    return h_bits(32, false);
}

HParser* root_dispersion_parser() {
    return h_bits(32, false);
}

HParser* reference_id_parser() {
    return h_bits(32, false);
}

HParser* timestamp_parser() {
    return h_sequence(h_bits(32, false), h_bits(32, false), NULL);
}

HParser* extension_field_parser() {
    return h_sequence(
        h_bits(16, false),  // Type
        h_bits(16, false),  // Length
        h_bits(16, false),  // Value
        NULL
    );
}

HParser* authenticator_parser() {
    return h_sequence(
        h_bits(32, false),  // Key ID
        h_bits(128, false), // Message Digest
        NULL
    );
}

HParser* create_ntp_parser() {
    return h_sequence(
        li_vn_mode_parser(),
        stratum_parser(),
        poll_parser(),
        precision_parser(),
        root_delay_parser(),
        root_dispersion_parser(),
        reference_id_parser(),
        timestamp_parser(),  // Reference Timestamp
        timestamp_parser(),  // Origin Timestamp
        timestamp_parser(),  // Receive Timestamp
        timestamp_parser(),  // Transmit Timestamp
        h_optional(extension_field_parser()),
        h_optional(authenticator_parser()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
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

    // Create and initialize parser
    HParser *ntp_parser = create_ntp_parser();
    if (!ntp_parser) {
        fprintf(stderr, "Failed to create parser\n");
        free(input);
        return 1;
    }

    // Parse input
    HParseResult *result = h_parse(ntp_parser, input, size);
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        return 1;
    }

    // Cleanup
    h_parse_result_free(result);
    free(input);
    return 0;
}