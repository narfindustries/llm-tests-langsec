#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ARP Parser
HParser* create_arp_parser(void) {
    // Hardware Type (HTYPE)
    HParser* htype = h_uint16();
    
    // Protocol Type (PTYPE)
    HParser* ptype = h_uint16();
    
    // Hardware Length (HLEN)
    HParser* hlen = h_uint8();
    
    // Protocol Length (PLEN)
    HParser* plen = h_uint8();
    
    // Operation (OPER)
    HParser* oper = h_uint16();
    
    // Sender Hardware Address (SHA)
    HParser* sha = h_repeat_n(h_uint8(), 6);  // MAC address is 6 bytes
    
    // Sender Protocol Address (SPA)
    HParser* spa = h_repeat_n(h_uint8(), 4);  // IPv4 address is 4 bytes
    
    // Target Hardware Address (THA)
    HParser* tha = h_repeat_n(h_uint8(), 6);  // MAC address is 6 bytes
    
    // Target Protocol Address (TPA)
    HParser* tpa = h_repeat_n(h_uint8(), 4);  // IPv4 address is 4 bytes
    
    return h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    // Get file size
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // Read file content
    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }
    
    if (fread(input, 1, size, fp) != size) {
        perror("Failed to read file");
        free(input);
        fclose(fp);
        return 1;
    }
    fclose(fp);

    // Create and run parser
    HParser *arp_parser = create_arp_parser();
    HParseResult *result = h_parse(arp_parser, input, size);

    if (!result) {
        fprintf(stderr, "Failed to parse ARP packet\n");
        free(input);
        return 1;
    }

    // Parse successful
    printf("ARP packet parsed successfully\n");

    // Clean up
    h_parse_result_free(result);
    free(input);
    return 0;
}