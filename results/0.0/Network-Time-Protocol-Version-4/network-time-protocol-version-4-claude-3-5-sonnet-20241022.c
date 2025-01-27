#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// NTP v4 Parser Combinators
HParser* create_ntp_parser() {
    // Basic fields
    HParser* li_vn_mode = h_bits(8, false);
    HParser* stratum = h_bits(8, false);
    HParser* poll = h_bits(8, false);
    HParser* precision = h_bits(8, false);
    HParser* root_delay = h_bits(32, false);
    HParser* root_dispersion = h_bits(32, false);
    HParser* reference_id = h_bits(32, false);
    HParser* reference_timestamp = h_bits(64, false);
    HParser* origin_timestamp = h_bits(64, false);
    HParser* receive_timestamp = h_bits(64, false);
    HParser* transmit_timestamp = h_bits(64, false);

    // Optional Extension Fields
    HParser* extension_field = h_sequence(
        h_bits(16, false),  // Field Type
        h_bits(16, false),  // Length
        h_many(h_bits(8, false)),  // Value
        NULL
    );
    HParser* extension_fields = h_many(extension_field);

    // Optional MAC
    HParser* mac = h_sequence(
        h_bits(16, false),  // Key ID
        h_bits(16, false),  // Length
        h_many(h_bits(8, false)),  // MAC
        NULL
    );

    // Complete NTP Packet
    return h_sequence(
        li_vn_mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
        extension_fields,
        h_optional(mac),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    // Parse NTP packet
    HParser *ntp_parser = create_ntp_parser();
    HParseResult *result = h_parse(ntp_parser, input, size);

    if (!result) {
        fprintf(stderr, "Failed to parse NTP packet\n");
        free(input);
        return 1;
    }

    // Cleanup
    h_parse_result_free(result);
    free(input);
    return 0;
}