#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define NTP message fields sizes
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
#define DIGEST_SIZE 128

// Create parsers for each field
HParser *ntp_li;
HParser *ntp_vn;
HParser *ntp_mode;
HParser *ntp_stratum;
HParser *ntp_poll;
HParser *ntp_precision;
HParser *ntp_root_delay;
HParser *ntp_root_dispersion;
HParser *ntp_ref_id;
HParser *ntp_ref_timestamp;
HParser *ntp_orig_timestamp;
HParser *ntp_recv_timestamp;
HParser *ntp_trans_timestamp;
HParser *ntp_key_id;
HParser *ntp_message_digest;

// NTP message parser
HParser *ntp_message;

void init_parsers() {
    ntp_li = h_bits(LI_SIZE, false);
    ntp_vn = h_bits(VN_SIZE, false);
    ntp_mode = h_bits(MODE_SIZE, false);
    ntp_stratum = h_bits(STRATUM_SIZE, false);
    ntp_poll = h_bits(POLL_SIZE, false);
    ntp_precision = h_bits(PRECISION_SIZE, false);
    ntp_root_delay = h_bits(ROOT_DELAY_SIZE, false);
    ntp_root_dispersion = h_bits(ROOT_DISPERSION_SIZE, false);
    ntp_ref_id = h_bits(REF_ID_SIZE, false);
    ntp_ref_timestamp = h_bits(TIMESTAMP_SIZE, false);
    ntp_orig_timestamp = h_bits(TIMESTAMP_SIZE, false);
    ntp_recv_timestamp = h_bits(TIMESTAMP_SIZE, false);
    ntp_trans_timestamp = h_bits(TIMESTAMP_SIZE, false);
    ntp_key_id = h_bits(OPTIONAL_FIELD_SIZE, false);
    ntp_message_digest = h_bits(DIGEST_SIZE, false);

    ntp_message = h_sequence(ntp_li, ntp_vn, ntp_mode, ntp_stratum, ntp_poll, ntp_precision,
                             ntp_root_delay, ntp_root_dispersion, ntp_ref_id, ntp_ref_timestamp,
                             ntp_orig_timestamp, ntp_recv_timestamp, ntp_trans_timestamp,
                             ntp_key_id, ntp_message_digest, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Initialize parsers
    init_parsers();

    // Read file into buffer
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    // Parse the buffer
    HParseResult *result = h_parse(ntp_message, buffer, file_size);
    if (result) {
        printf("NTP packet parsed successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse NTP packet.\n");
    }

    // Clean up
    free(buffer);
    h_parse_result_free(result);

    return 0;
}