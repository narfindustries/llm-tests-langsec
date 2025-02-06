#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define bit sizes for easy readability
#define BIT_2 2
#define BIT_3 3
#define BIT_8 8
#define BIT_32 32
#define BIT_64 64

// Parser for NTPv4 Packet according to RFC 5905
HParser *ntp_parser() {
    HParser *p_li = h_bits(BIT_2, false);
    HParser *p_vn = h_bits(BIT_3, false);
    HParser *p_mode = h_bits(BIT_3, false);
    HParser *p_stratum = h_uint8();
    HParser *p_poll = h_int8();
    HParser *p_precision = h_int8();
    HParser *p_root_delay = h_bits(BIT_32, false);
    HParser *p_root_dispersion = h_bits(BIT_32, false);
    HParser *p_reference_identifier = h_bits(BIT_32, false);
    HParser *p_reference_timestamp = h_bits(BIT_64, false);
    HParser *p_originate_timestamp = h_bits(BIT_64, false);
    HParser *p_receive_timestamp = h_bits(BIT_64, false);
    HParser *p_transmit_timestamp = h_bits(BIT_64, false);
    
    HParser *p_extension_fields = h_many(h_uint8());
    HParser *p_key_identifier = h_optional(h_bits(BIT_32, false));
    HParser *p_message_digest = h_optional(h_bits(128, false)); // changed h_bytes(16) to h_bits(128, false)

    return h_sequence(p_li, p_vn, p_mode, p_stratum, p_poll, p_precision,
                      p_root_delay, p_root_dispersion, p_reference_identifier,
                      p_reference_timestamp, p_originate_timestamp, p_receive_timestamp,
                      p_transmit_timestamp, p_extension_fields, p_key_identifier,
                      p_message_digest, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <NTPv4_Binary_File>\n", argv[0]);
        return 1;
    }

    FILE *input_file = fopen(argv[1], "rb");
    if (!input_file) {
        perror("Error opening the file");
        return 1;
    }

    fseek(input_file, 0, SEEK_END);
    long input_size = ftell(input_file);
    fseek(input_file, 0, SEEK_SET);

    uint8_t *data = malloc(input_size);
    if (!data) {
        perror("Memory allocation failed");
        fclose(input_file);
        return 1;
    }

    fread(data, 1, input_size, input_file);
    fclose(input_file);

    HParser *ntp = ntp_parser();
    HParseResult *result = h_parse(ntp, data, input_size);

    if (result) {
        printf("NTP packet parsed successfully!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "Failed to parse the NTP packet.\n");
    }

    h_parse_result_free(result);
    h_free_parser(ntp);
    free(data);

    return 0;
}