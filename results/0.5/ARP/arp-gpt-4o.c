#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *arp_parser() {
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    HParser *sha = h_length_value(hlen, h_uint8());
    HParser *spa = h_length_value(plen, h_uint8());
    HParser *tha = h_length_value(hlen, h_uint8());
    HParser *tpa = h_length_value(plen, h_uint8());

    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

void parse_arp(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = (unsigned char *)malloc(file_size);
    if (!data) {
        perror("Memory allocation failed");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = arp_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("ARP packet parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ARP packet.\n");
    }

    free(data);
    h_delete(parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_arp(argv[1]);
    return EXIT_SUCCESS;
}