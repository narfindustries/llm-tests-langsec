#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ICMP Parser Combinators
HParser* create_icmp_parser(void) {
    // Basic fields
    HParser* type = h_uint8();
    HParser* code = h_uint8();
    HParser* checksum = h_uint16();

    // Rest of header based on type
    HParser* identifier = h_uint16();
    HParser* sequence = h_uint16();
    HParser* unused = h_uint32();
    HParser* originate_timestamp = h_uint32();
    HParser* receive_timestamp = h_uint32();
    HParser* transmit_timestamp = h_uint32();
    HParser* gateway_internet_address = h_uint32();
    HParser* pointer = h_uint8();
    HParser* num_addrs = h_uint8();
    HParser* addr_entry_size = h_uint8();
    HParser* lifetime = h_uint16();

    // Address entry parser for Router Advertisement
    HParser* router_addr = h_uint32();
    HParser* preference_level = h_uint32();
    HParser* addr_entry = h_sequence(router_addr, preference_level, NULL);
    
    // Variable length address entries
    HParser* addr_entries = h_repeat_n(addr_entry, 0);

    // Optional data field
    HParser* optional_data = h_many(h_uint8());

    // Combine all fields based on type
    return h_sequence(type, code, checksum,
                     h_optional(identifier),
                     h_optional(sequence),
                     h_optional(unused),
                     h_optional(originate_timestamp),
                     h_optional(receive_timestamp),
                     h_optional(transmit_timestamp),
                     h_optional(gateway_internet_address),
                     h_optional(pointer),
                     h_optional(num_addrs),
                     h_optional(addr_entry_size),
                     h_optional(lifetime),
                     h_optional(addr_entries),
                     h_optional(optional_data),
                     NULL);
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

    // Parse ICMP packet
    HParser *parser = create_icmp_parser();
    HParseResult *result = h_parse(parser, input, size);

    if (!result) {
        fprintf(stderr, "Failed to parse ICMP packet\n");
        free(input);
        return 1;
    }

    // Clean up
    h_parse_result_free(result);
    free(input);
    return 0;
}