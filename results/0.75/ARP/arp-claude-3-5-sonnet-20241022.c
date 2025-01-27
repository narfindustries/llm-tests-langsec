#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *init_arp_parser(void) {
    // Hardware type (HTYPE)
    HParser *htype = h_uint16();
    
    // Protocol type (PTYPE)
    HParser *ptype = h_uint16();
    
    // Hardware length (HLEN)
    HParser *hlen = h_uint8();
    
    // Protocol length (PLEN)
    HParser *plen = h_uint8();
    
    // Operation (OPER)
    HParser *oper = h_uint16();
    
    // Sender hardware address (SHA)
    HParser *sha = h_repeat_n(h_uint8(), 6);  // Assuming Ethernet MAC (6 bytes)
    
    // Sender protocol address (SPA)
    HParser *spa = h_repeat_n(h_uint8(), 4);  // Assuming IPv4 (4 bytes)
    
    // Target hardware address (THA)
    HParser *tha = h_repeat_n(h_uint8(), 6);  // Assuming Ethernet MAC (6 bytes)
    
    // Target protocol address (TPA)
    HParser *tpa = h_repeat_n(h_uint8(), 4);  // Assuming IPv4 (4 bytes)
    
    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
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
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, size, file) != size) {
        perror("Failed to read file");
        free(input);
        fclose(file);
        return 1;
    }

    HParser *arp_parser = init_arp_parser();
    HParseResult *result = h_parse(arp_parser, input, size);

    if (result) {
        printf("Successfully parsed ARP packet\n");
        // Access parsed data through result->ast
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ARP packet\n");
    }

    free(input);
    fclose(file);
    return 0;
}