#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_addr_len;
    uint8_t protocol_addr_len;
    uint16_t operation;
    uint8_t *sender_hardware_addr;
    uint8_t *sender_protocol_addr;
    uint8_t *target_hardware_addr;
    uint8_t *target_protocol_addr;
} arp_packet;

static HParsedToken* act_arp(const HParseResult *p, void* user_data) {
    arp_packet *result = malloc(sizeof(arp_packet));
    
    const HParsedToken *seq = p->ast;
    
    result->hardware_type = seq->seq->elements[0]->uint;
    result->protocol_type = seq->seq->elements[1]->uint;
    result->hardware_addr_len = seq->seq->elements[2]->uint;
    result->protocol_addr_len = seq->seq->elements[3]->uint;
    result->operation = seq->seq->elements[4]->uint;
    
    result->sender_hardware_addr = malloc(6);
    memcpy(result->sender_hardware_addr, seq->seq->elements[5]->bytes.token, 6);
    
    result->sender_protocol_addr = malloc(4);
    memcpy(result->sender_protocol_addr, seq->seq->elements[6]->bytes.token, 4);
    
    result->target_hardware_addr = malloc(6);
    memcpy(result->target_hardware_addr, seq->seq->elements[7]->bytes.token, 6);
    
    result->target_protocol_addr = malloc(4);
    memcpy(result->target_protocol_addr, seq->seq->elements[8]->bytes.token, 4);
    
    HParsedToken *token = h_new_token(TT_SEQUENCE, p->ast->token_type);
    token->user = result;
    return token;
}

void print_arp_packet(arp_packet *pkt) {
    printf("Hardware Type: 0x%04x\n", pkt->hardware_type);
    printf("Protocol Type: 0x%04x\n", pkt->protocol_type);
    printf("Hardware Address Length: %u\n", pkt->hardware_addr_len);
    printf("Protocol Address Length: %u\n", pkt->protocol_addr_len);
    printf("Operation: %u\n", pkt->operation);
    
    printf("Sender Hardware Address: ");
    for(int i = 0; i < 6; i++) {
        printf("%02x:", pkt->sender_hardware_addr[i]);
    }
    printf("\n");
    
    printf("Sender Protocol Address: ");
    for(int i = 0; i < 4; i++) {
        printf("%u.", pkt->sender_protocol_addr[i]);
    }
    printf("\n");
    
    printf("Target Hardware Address: ");
    for(int i = 0; i < 6; i++) {
        printf("%02x:", pkt->target_hardware_addr[i]);
    }
    printf("\n");
    
    printf("Target Protocol Address: ");
    for(int i = 0; i < 4; i++) {
        printf("%u.", pkt->target_protocol_addr[i]);
    }
    printf("\n");
}

int main(int argc, char *argv[]) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if(!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *buf = malloc(size);
    if(fread(buf, 1, size, f) != size) {
        perror("Failed to read file");
        fclose(f);
        free(buf);
        return 1;
    }
    fclose(f);

    HParser *hardware_type = h_uint16();
    HParser *protocol_type = h_uint16();
    HParser *hardware_addr_len = h_uint8();
    HParser *protocol_addr_len = h_uint8();
    HParser *operation = h_uint16();
    HParser *sender_hardware_addr = h_bits(48, false);
    HParser *sender_protocol_addr = h_bits(32, false);
    HParser *target_hardware_addr = h_bits(48, false);
    HParser *target_protocol_addr = h_bits(32, false);

    HParser *arp = h_sequence(hardware_type,
                            protocol_type,
                            hardware_addr_len,
                            protocol_addr_len,
                            operation,
                            sender_hardware_addr,
                            sender_protocol_addr,
                            target_hardware_addr,
                            target_protocol_addr,
                            NULL);

    arp = h_action(arp, act_arp, NULL);

    HParseResult *result = h_parse(arp, buf, size);

    if(result) {
        arp_packet *pkt = (arp_packet*)result->ast->user;
        print_arp_packet(pkt);
        
        free(pkt->sender_hardware_addr);
        free(pkt->sender_protocol_addr);
        free(pkt->target_hardware_addr);
        free(pkt->target_protocol_addr);
        free(pkt);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ARP packet\n");
    }

    free(buf);
    return 0;
}