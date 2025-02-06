#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// NTP Packet Structure in Hammer
HParser *ntp_packet(void) {
    HParser *li_vn_mode = h_bits(8, false);
    HParser *stratum = h_uint8();
    HParser *poll = h_uint8();
    HParser *precision = h_int8();
    HParser *root_delay = h_uint32();
    HParser *root_dispersion = h_uint32();
    HParser *reference_id = h_uint32();
    HParser *reference_timestamp = h_uint64();
    HParser *origin_timestamp = h_uint64();
    HParser *receive_timestamp = h_uint64();
    HParser *transmit_timestamp = h_uint64();

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
        NULL
    );
}

void print_ntp_packet(const HParsedToken *parsed) {
    const uint8_t li_vn_mode = parsed->seq->elements[0]->uint;
    const uint8_t stratum = parsed->seq->elements[1]->uint;
    const uint8_t poll = parsed->seq->elements[2]->uint;
    const int8_t precision = parsed->seq->elements[3]->sint;
    const uint32_t root_delay = parsed->seq->elements[4]->uint;
    const uint32_t root_dispersion = parsed->seq->elements[5]->uint;
    const uint32_t reference_id = parsed->seq->elements[6]->uint;
    const uint64_t reference_timestamp = parsed->seq->elements[7]->uint;
    const uint64_t origin_timestamp = parsed->seq->elements[8]->uint;
    const uint64_t receive_timestamp = parsed->seq->elements[9]->uint;
    const uint64_t transmit_timestamp = parsed->seq->elements[10]->uint;

    printf("LI_VN_MODE: %u\n", li_vn_mode);
    printf("Stratum: %u\n", stratum);
    printf("Poll: %u\n", poll);
    printf("Precision: %d\n", precision);
    printf("Root Delay: %u\n", root_delay);
    printf("Root Dispersion: %u\n", root_dispersion);
    printf("Reference ID: %u\n", reference_id);
    printf("Reference Timestamp: %llu\n", reference_timestamp);
    printf("Origin Timestamp: %llu\n", origin_timestamp);
    printf("Receive Timestamp: %llu\n", receive_timestamp);
    printf("Transmit Timestamp: %llu\n", transmit_timestamp);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fsize = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buff = malloc(fsize);
    if (!buff) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buff, 1, fsize, file) != fsize) {
        perror("Failed to read file");
        free(buff);
        fclose(file);
        return 1;
    }

    fclose(file);

    HParser *parser = ntp_packet();
    HParseResult *result = h_parse(parser, buff, fsize);

    if (result && result->ast) {
        print_ntp_packet(result->ast);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse NTP packet.\n");
    }

    h_delete(parser);
    free(buff);

    return 0;
}