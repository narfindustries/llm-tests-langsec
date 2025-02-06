#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define bit sizes for various fields
#define LI_SIZE 2
#define VN_SIZE 3
#define MODE_SIZE 3
#define STRATUM_SIZE 8
#define POLL_SIZE 8
#define PRECISION_SIZE 8
#define ROOT_DELAY_SIZE 32
#define ROOT_DISPERSION_SIZE 32
#define REF_ID_SIZE 32
#define TIMESTAMP_SIZE 64
#define OPTIONAL_FIELD_SIZE 32
#define MESSAGE_DIGEST_SIZE 128

// Function to parse NTP packet
HParser *ntp_parser() {
    HParser *li = h_bits(LI_SIZE, false);
    HParser *vn = h_bits(VN_SIZE, false);
    HParser *mode = h_bits(MODE_SIZE, false);
    HParser *stratum = h_bits(STRATUM_SIZE, false);
    HParser *poll = h_bits(POLL_SIZE, false);
    HParser *precision = h_bits(PRECISION_SIZE, false);
    HParser *root_delay = h_bits(ROOT_DELAY_SIZE, false);
    HParser *root_dispersion = h_bits(ROOT_DISPERSION_SIZE, false);
    HParser *ref_id = h_bits(REF_ID_SIZE, false);
    HParser *ref_timestamp = h_bits(TIMESTAMP_SIZE, false);
    HParser *orig_timestamp = h_bits(TIMESTAMP_SIZE, false);
    HParser *recv_timestamp = h_bits(TIMESTAMP_SIZE, false);
    HParser *trans_timestamp = h_bits(TIMESTAMP_SIZE, false);
    HParser *extension_fields = h_many(h_bits(OPTIONAL_FIELD_SIZE, false)); // Assuming variable length as multiple of 32 bits
    HParser *key_id = h_optional(h_bits(OPTIONAL_FIELD_SIZE, false));
    HParser *message_digest = h_optional(h_bits(MESSAGE_DIGEST_SIZE, false));

    return h_sequence(li, vn, mode, stratum, poll, precision, root_delay, root_dispersion,
                      ref_id, ref_timestamp, orig_timestamp, recv_timestamp, trans_timestamp,
                      extension_fields, key_id, message_digest, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to determine the file size
    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read the entire file into memory
    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, length, file) != length) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    // Parse the NTP packet
    HParser *parser = ntp_parser();
    HParseResult *result = h_parse(parser, data, length);
    if (result) {
        printf("NTP packet parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse NTP packet.\n");
    }

    // Clean up
    h_parse_result_free(result);
    free(data);

    return 0;
}