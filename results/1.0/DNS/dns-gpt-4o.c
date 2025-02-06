#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser* create_dns_parser() {
    HParser *uint16be = h_uint16(); // Use h_uint16() for big-endian 16-bit values
    HParser *uint32be = h_uint32(); // Use h_uint32() for big-endian 32-bit values

    HParser *dns_header = h_sequence(
        uint16be,                                // ID
        h_bits(1, false),                        // QR: 1 bit
        h_bits(4, false),                        // Opcode: 4 bits
        h_bits(1, false),                        // AA: 1 bit
        h_bits(1, false),                        // TC: 1 bit
        h_bits(1, false),                        // RD: 1 bit
        h_bits(1, false),                        // RA: 1 bit
        h_bits(3, false),                        // Z: 3 bits
        h_bits(4, false),                        // RCODE: 4 bits
        uint16be,                                // QDCOUNT
        uint16be,                                // ANCOUNT
        uint16be,                                // NSCOUNT
        uint16be,                                // ARCOUNT
        NULL
    );

    HParser *label = h_sequence(
        h_uint8(),                               // Length
        h_data(h_uint8())                        // Label data
    );

    HParser *name = h_many1(label);

    HParser *query_section = h_many(
        h_sequence(
            name,                                 // QNAME
            uint16be,                             // QTYPE
            uint16be                              // QCLASS
        )
    );

    HParser *resource_record = h_many(
        h_sequence(
            name,                                 // NAME
            uint16be,                             // TYPE
            uint16be,                             // CLASS
            uint32be,                             // TTL
            h_bind(uint16be, h_nodata, h_repeat_n, h_last_uint),
            NULL
        )
    );

    HParser *dns_message = h_sequence(
        dns_header,
        query_section,
        resource_record, // Answer section
        resource_record, // Authority section
        resource_record, // Additional section
        NULL
    );

    return dns_message;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("fopen");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(filesize);
    fread(data, 1, filesize, file);
    fclose(file);

    HParser *parser = create_dns_parser();
    HParseResult *result = h_parse(parser, data, filesize);

    if (result->ast) {
        printf("Parsing successful!\n");
        // Handle parsed AST here, if needed
    } else {
        printf("Parsing failed.\n");
    }

    h_parse_result_free(result);
    h_arena_free(result->arena);
    h_parser_free(parser);
    free(data);

    return 0;
}