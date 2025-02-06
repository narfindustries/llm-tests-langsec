#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    const uint8_t *sha;
    const uint8_t *spa;
    const uint8_t *tha;
    const uint8_t *tpa;
} arp_packet;

static HParser* init_arp_parser(void) {
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    
    // For MAC addresses (6 bytes) and IP addresses (4 bytes)
    HParser *sha = h_repeat_n(h_uint8(), 6);
    HParser *spa = h_repeat_n(h_uint8(), 4);
    HParser *tha = h_repeat_n(h_uint8(), 6);
    HParser *tpa = h_repeat_n(h_uint8(), 4);
    
    return h_sequence(htype, ptype, hlen, plen, oper, 
                     sha, spa, tha, tpa, NULL);
}

static void print_bytes(const uint8_t *data, size_t len) {
    for(size_t i = 0; i < len; i++) {
        printf("%02x", data[i]);
    }
    printf("\n");
}

static void process_parse_result(HParseResult *result) {
    if(!result) {
        printf("Failed to parse ARP packet\n");
        return;
    }
    
    const HCountedArray *seq = result->ast->seq;
    arp_packet packet = {
        .htype = seq->elements[0]->uint,
        .ptype = seq->elements[1]->uint,
        .hlen = seq->elements[2]->uint,
        .plen = seq->elements[3]->uint,
        .oper = seq->elements[4]->uint,
        .sha = seq->elements[5]->seq->elements,
        .spa = seq->elements[6]->seq->elements,
        .tha = seq->elements[7]->seq->elements,
        .tpa = seq->elements[8]->seq->elements
    };
    
    printf("Hardware Type: 0x%04x\n", packet.htype);
    printf("Protocol Type: 0x%04x\n", packet.ptype);
    printf("Hardware Length: %u\n", packet.hlen);
    printf("Protocol Length: %u\n", packet.plen);
    printf("Operation: %u\n", packet.oper);
    
    printf("Sender Hardware Address: ");
    print_bytes(packet.sha, 6);
    
    printf("Sender Protocol Address: ");
    print_bytes(packet.spa, 4);
    
    printf("Target Hardware Address: ");
    print_bytes(packet.tha, 6);
    
    printf("Target Protocol Address: ");
    print_bytes(packet.tpa, 4);
}

int main(int argc, char *argv[]) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
        return 1;
    }
    
    FILE *fp = fopen(argv[1], "rb");
    if(!fp) {
        perror("Failed to open file");
        return 1;
    }
    
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    
    uint8_t *data = malloc(size);
    if(!data) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }
    
    if(fread(data, 1, size, fp) != size) {
        perror("Failed to read file");
        free(data);
        fclose(fp);
        return 1;
    }
    
    HParser *parser = init_arp_parser();
    HParseResult *result = h_parse(parser, data, size);
    
    process_parse_result(result);
    
    h_parse_result_free(result);
    free(data);
    fclose(fp);
    return 0;
}