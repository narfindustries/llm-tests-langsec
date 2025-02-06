#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_arp_parser() {
    // Define ARP parsing components
    HParser *h_type = h_uint16();
    HParser *p_type = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    
    // Capture the length values
    HParser *sha_len = h_bind(hlen, h_repeat_n, h_uint8());
    HParser *spa_len = h_bind(plen, h_repeat_n, h_uint8());
    HParser *tha_len = h_bind(hlen, h_repeat_n, h_uint8());
    HParser *tpa_len = h_bind(plen, h_repeat_n, h_uint8());

    // Construct ARP packet parser
    HParser *arp_parser = h_sequence(
        h_type, // Hardware Type
        p_type, // Protocol Type
        hlen,   // Hardware Address Length
        plen,   // Protocol Address Length
        oper,   // Operation
        sha_len, // Sender Hardware Address
        spa_len, // Sender Protocol Address
        tha_len, // Target Hardware Address
        tpa_len, // Target Protocol Address
        NULL
    );

    return arp_parser;
}

void parse_arp(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = (unsigned char *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *arp_parser = create_arp_parser();
    HParseResult *result = h_parse(arp_parser, data, file_size);

    if (result) {
        printf("ARP packet parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ARP packet.\n");
    }

    h_parser_unref(arp_parser);
    free(data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_arp(argv[1]);
    return EXIT_SUCCESS;
}