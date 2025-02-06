#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t leap_indicator;
    uint8_t version;
    uint8_t mode;
    uint8_t stratum;
    int8_t poll;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
    HParsedToken* authenticator;
} NTPPacket;

static HParser* ntp_parser() {
    HParser* leap_indicator = h_bits(2, false);
    HParser* version = h_bits(3, false);
    HParser* mode = h_bits(3, false);
    HParser* stratum = h_bits(8, false);
    HParser* poll = h_int8();
    HParser* precision = h_int8();
    HParser* root_delay = h_uint32();
    HParser* root_dispersion = h_uint32();
    HParser* reference_id = h_uint32();
    HParser* reference_timestamp = h_uint64();
    HParser* originate_timestamp = h_uint64();
    HParser* receive_timestamp = h_uint64();
    HParser* transmit_timestamp = h_uint64();
    HParser* authenticator = h_optional(h_many(h_uint8()));

    return h_sequence(
        leap_indicator,
        version,
        mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
        authenticator,
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ntp_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = ntp_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("NTP Packet parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    free(buffer);
    h_parser_free(parser);
    return 0;
}