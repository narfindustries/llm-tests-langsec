#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *arp_parser() {
    // Define the ARP packet fields
    HParser *htype = h_uint16(); // Hardware Type
    HParser *ptype = h_uint16(); // Protocol Type
    HParser *hlen = h_uint8();   // Hardware Address Length
    HParser *plen = h_uint8();   // Protocol Address Length
    HParser *oper = h_uint16();  // Operation

    // Hardware and Protocol addresses are variable length
    HParser *sha = h_length_value(hlen, h_uint8()); // Sender Hardware Address
    HParser *spa = h_length_value(plen, h_uint8()); // Sender Protocol Address
    HParser *tha = h_length_value(hlen, h_uint8()); // Target Hardware Address
    HParser *tpa = h_length_value(plen, h_uint8()); // Target Protocol Address

    // Combine all fields into an ARP packet parser
    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

void print_bytes(const uint8_t *data, size_t length) {
    for (size_t i = 0; i < length; ++i) {
        printf("%02x ", data[i]);
    }
    printf("\n");
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = arp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result->ast) {
        printf("ARP Packet Parsed Successfully:\n");
        print_bytes(buffer, file_size);
    } else {
        fprintf(stderr, "Failed to parse ARP packet\n");
    }

    h_parse_result_free(result);
    hobj_unref(parser); // Correct function to free parser
    free(buffer);

    return EXIT_SUCCESS;
}