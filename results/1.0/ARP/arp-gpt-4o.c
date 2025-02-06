#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *arp_packet_parser() {
    // Define parsers for all fields of the ARP packet
    HParser *htype = h_uint16();   // Hardware Type
    HParser *ptype = h_uint16();   // Protocol Type
    HParser *hlen = h_uint8();     // Hardware Length
    HParser *plen = h_uint8();     // Protocol Length
    HParser *oper = h_uint16();    // Operation
    HParser *sha = h_repeat_n(h_uint8(), 6);  // Sender Hardware Address (Ethernet uses 6 bytes)
    HParser *spa = h_repeat_n(h_uint8(), 4);  // Sender Protocol Address (IPv4 uses 4 bytes)
    HParser *tha = h_repeat_n(h_uint8(), 6);  // Target Hardware Address (Ethernet uses 6 bytes)
    HParser *tpa = h_repeat_n(h_uint8(), 4);  // Target Protocol Address (IPv4 uses 4 bytes)

    // Aggregate all parsers into a sequence that matches the ARP packet structure
    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

void parse_arp_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Could not open file %s\n", filename);
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long fsize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fsize);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    if (fread(buffer, 1, fsize, file) != fsize) {
        fprintf(stderr, "Error reading file %s\n", filename);
        free(buffer);
        fclose(file);
        exit(EXIT_FAILURE);
    }
    fclose(file);

    HParser *arp_parser = arp_packet_parser();
    HParseResult *result = h_parse(arp_parser, buffer, fsize);

    if (result->bit_length >= 0) {
        printf("ARP packet parsed successfully.\n");
        // Display parsed fields (this can be expanded or refined as needed)
    } else {
        fprintf(stderr, "Failed to parse ARP packet.\n");
    }

    h_parse_result_free(result);
    free(buffer);
    // No need to explicitly free the parser itself with Hammer; parsers are managed automatically
    // If you do need to dynamically free a custom parser, it might require a different approach
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_arp_file(argv[1]);

    return EXIT_SUCCESS;
}