#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

// ICMP message parsing
HParser* create_icmp_parser() {
    HParser* icmp_type_parser = h_uint8();
    HParser* icmp_code_parser = h_uint8();
    HParser* icmp_checksum_parser = h_uint16();

    HParser* icmp_echo_parser = h_sequence(
        h_uint16(),  // Identifier
        h_uint16(),  // Sequence Number
        NULL
    );

    HParser* icmp_timestamp_parser = h_sequence(
        h_uint16(),  // Identifier
        h_uint16(),  // Sequence Number
        h_uint32(),  // Originate Timestamp
        h_uint32(),  // Receive Timestamp
        h_uint32(),  // Transmit Timestamp
        NULL
    );

    HParser* icmp_address_mask_parser = h_sequence(
        h_uint16(),  // Identifier
        h_uint16(),  // Sequence Number
        h_uint32(),  // Address Mask
        NULL
    );

    HParser* icmp_message_parser = h_choice(
        h_sequence(icmp_type_parser, h_value(0, icmp_echo_parser), NULL),  // Echo Reply
        h_sequence(icmp_type_parser, h_value(8, icmp_echo_parser), NULL),  // Echo Request
        h_sequence(icmp_type_parser, h_value(13, icmp_timestamp_parser), NULL),  // Timestamp
        h_sequence(icmp_type_parser, h_value(14, icmp_timestamp_parser), NULL),  // Timestamp Reply
        h_sequence(icmp_type_parser, h_value(17, icmp_address_mask_parser), NULL),  // Address Mask Request
        h_sequence(icmp_type_parser, h_value(18, icmp_address_mask_parser), NULL),  // Address Mask Reply
        NULL
    );

    return h_sequence(
        icmp_type_parser,
        icmp_code_parser,
        icmp_checksum_parser,
        icmp_message_parser,
        NULL
    );
}

void parse_icmp(const uint8_t *data, size_t length, HParser* icmp_parser) {
    HParseResult *result = h_parse(icmp_parser, data, length);
    if (result) {
        h_dump(stdout, result->ast);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ICMP message.\n");
    }
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
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser* icmp_parser = create_icmp_parser();
    parse_icmp(data, file_size, icmp_parser);

    free(data);
    return EXIT_SUCCESS;
}