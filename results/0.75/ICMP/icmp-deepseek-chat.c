#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ICMP Header Structure
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t id;
            uint16_t sequence;
        } echo;
        uint32_t gateway;
        struct {
            uint16_t unused;
            uint16_t mtu;
        } frag;
    } un;
} icmp_header_t;

// Parser for ICMP Type
HParser *icmp_type_parser() {
    return h_uint8();
}

// Parser for ICMP Code
HParser *icmp_code_parser() {
    return h_uint8();
}

// Parser for ICMP Checksum
HParser *icmp_checksum_parser() {
    return h_uint16();
}

// Parser for ICMP Echo ID
HParser *icmp_echo_id_parser() {
    return h_uint16();
}

// Parser for ICMP Echo Sequence
HParser *icmp_echo_sequence_parser() {
    return h_uint16();
}

// Parser for ICMP Gateway
HParser *icmp_gateway_parser() {
    return h_uint32();
}

// Parser for ICMP Unused Field
HParser *icmp_unused_parser() {
    return h_uint16();
}

// Parser for ICMP MTU
HParser *icmp_mtu_parser() {
    return h_uint16();
}

// Parser for ICMP Header
HParser *icmp_header_parser() {
    return h_sequence(
        icmp_type_parser(),
        icmp_code_parser(),
        icmp_checksum_parser(),
        h_choice(
            4,
            h_sequence(icmp_echo_id_parser(), icmp_echo_sequence_parser()),
            icmp_gateway_parser(),
            h_sequence(icmp_unused_parser(), icmp_mtu_parser()),
            h_sequence(icmp_unused_parser(), icmp_mtu_parser())
        ),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = icmp_header_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ICMP header\n");
        free(buffer);
        return 1;
    }

    icmp_header_t *icmp_header = (icmp_header_t *)result->ast;
    printf("ICMP Type: %u\n", icmp_header->type);
    printf("ICMP Code: %u\n", icmp_header->code);
    printf("ICMP Checksum: %u\n", icmp_header->checksum);

    if (icmp_header->type == 0 || icmp_header->type == 8) {
        printf("Echo ID: %u\n", icmp_header->un.echo.id);
        printf("Echo Sequence: %u\n", icmp_header->un.echo.sequence);
    } else if (icmp_header->type == 5) {
        printf("Gateway: %u\n", icmp_header->un.gateway);
    } else if (icmp_header->type == 3 && icmp_header->code == 4) {
        printf("Unused: %u\n", icmp_header->un.frag.unused);
        printf("MTU: %u\n", icmp_header->un.frag.mtu);
    }

    h_parse_result_free(result);
    free(buffer);

    return 0;
}