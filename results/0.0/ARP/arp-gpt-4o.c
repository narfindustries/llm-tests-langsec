#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *arp_parser() {
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    
    // Dynamic length fields based on hlen and plen
    HParser *sha = h_length_value(hlen, h_uint8());
    HParser *spa = h_length_value(plen, h_uint8());
    HParser *tha = h_length_value(hlen, h_uint8());
    HParser *tpa = h_length_value(plen, h_uint8());

    // ARP packet structure
    return h_sequence(
        htype,
        ptype,
        hlen,
        plen,
        oper,
        sha,
        spa,
        tha,
        tpa,
        NULL
    );
}

void print_arp_info(HParsedToken *parsed) {
    if (!parsed || parsed->token_type != TT_SEQUENCE) {
        printf("Invalid ARP packet\n");
        return;
    }

    HParsedToken **fields = parsed->seq->elements;
    printf("Hardware Type: %u\n", fields[0]->uint);
    printf("Protocol Type: %u\n", fields[1]->uint);
    printf("Hardware Address Length: %u\n", fields[2]->uint);
    printf("Protocol Address Length: %u\n", fields[3]->uint);
    printf("Operation: %u\n", fields[4]->uint);

    printf("Sender Hardware Address: ");
    for (size_t i = 0; i < fields[5]->bytes.len; i++) {
        printf("%02x", fields[5]->bytes.token[i]);
        if (i < fields[5]->bytes.len - 1) printf(":");
    }
    printf("\n");

    printf("Sender Protocol Address: ");
    for (size_t i = 0; i < fields[6]->bytes.len; i++) {
        printf("%u", fields[6]->bytes.token[i]);
        if (i < fields[6]->bytes.len - 1) printf(".");
    }
    printf("\n");

    printf("Target Hardware Address: ");
    for (size_t i = 0; i < fields[7]->bytes.len; i++) {
        printf("%02x", fields[7]->bytes.token[i]);
        if (i < fields[7]->bytes.len - 1) printf(":");
    }
    printf("\n");

    printf("Target Protocol Address: ");
    for (size_t i = 0; i < fields[8]->bytes.len; i++) {
        printf("%u", fields[8]->bytes.token[i]);
        if (i < fields[8]->bytes.len - 1) printf(".");
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

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = arp_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result && result->ast) {
        print_arp_info(result->ast);
    } else {
        printf("Failed to parse ARP packet\n");
    }

    h_parse_result_free(result);
    free(data);
    return EXIT_SUCCESS;
}