#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// NTP v4 Parser using Hammer

// Helper parsers for fixed-width fields
static HParser* ntp_flags() {
    return h_bits(8, false);
}

static HParser* ntp_stratum() {
    return h_bits(8, false);
}

static HParser* ntp_poll() {
    return h_bits(8, false);
}

static HParser* ntp_precision() {
    return h_bits(8, false);
}

static HParser* ntp_root_delay() {
    return h_bits(32, false);
}

static HParser* ntp_root_dispersion() {
    return h_bits(32, false);
}

static HParser* ntp_ref_id() {
    return h_bits(32, false);
}

static HParser* ntp_timestamp() {
    return h_sequence(h_bits(32, false), h_bits(32, false), NULL);
}

static HParser* ntp_extension_field() {
    return h_sequence(
        h_bits(16, false),  // Type
        h_bits(16, false),  // Length
        h_length_value(h_bits(16, false), h_uint8()),  // Value
        NULL
    );
}

static HParser* ntp_mac() {
    return h_sequence(
        h_bits(16, false),  // Length
        h_length_value(h_bits(16, false), h_uint8()),  // Value
        NULL
    );
}

static HParser* ntp_packet() {
    return h_sequence(
        ntp_flags(),
        ntp_stratum(),
        ntp_poll(),
        ntp_precision(),
        ntp_root_delay(),
        ntp_root_dispersion(),
        ntp_ref_id(),
        ntp_timestamp(),  // Reference Timestamp
        ntp_timestamp(),  // Origin Timestamp
        ntp_timestamp(),  // Receive Timestamp
        ntp_timestamp(),  // Transmit Timestamp
        h_optional(h_many(ntp_extension_field())),
        h_optional(ntp_mac()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

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

    HParser *ntp_parser = ntp_packet();
    HParseResult *result = h_parse(ntp_parser, input, size);

    if (result) {
        printf("Successfully parsed NTP packet\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse NTP packet\n");
    }

    free(input);
    fclose(f);
    return 0;
}