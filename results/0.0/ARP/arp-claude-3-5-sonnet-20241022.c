#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_addr_len;
    uint8_t protocol_addr_len;
    uint16_t operation;
    uint8_t* sender_hardware_addr;
    uint8_t* sender_protocol_addr;
    uint8_t* target_hardware_addr;
    uint8_t* target_protocol_addr;
} arp_packet;

static HParser* init_arp_parser(void) {
    HParser* hardware_type = h_uint16();
    HParser* protocol_type = h_uint16();
    HParser* hardware_addr_len = h_uint8();
    HParser* protocol_addr_len = h_uint8();
    HParser* operation = h_uint16();
    
    // Variable length fields based on length fields
    HParser* sender_hardware_addr = h_length_value(hardware_addr_len, h_uint8());
    HParser* sender_protocol_addr = h_length_value(protocol_addr_len, h_uint8());
    HParser* target_hardware_addr = h_length_value(hardware_addr_len, h_uint8());
    HParser* target_protocol_addr = h_length_value(protocol_addr_len, h_uint8());

    return h_sequence(hardware_type, protocol_type, hardware_addr_len, 
                     protocol_addr_len, operation, sender_hardware_addr,
                     sender_protocol_addr, target_hardware_addr, 
                     target_protocol_addr, NULL);
}

static void print_bytes(uint8_t* data, size_t len) {
    for(size_t i = 0; i < len; i++) {
        printf("%02x", data[i]);
    }
    printf("\n");
}

static void print_arp_packet(const HParsedToken* token) {
    if(token->token_type != TT_SEQUENCE) return;
    
    printf("Hardware Type: 0x%04x\n", (uint16_t)token->seq->elements[0]->uint);
    printf("Protocol Type: 0x%04x\n", (uint16_t)token->seq->elements[1]->uint);
    printf("Hardware Address Length: %u\n", (uint8_t)token->seq->elements[2]->uint);
    printf("Protocol Address Length: %u\n", (uint8_t)token->seq->elements[3]->uint);
    printf("Operation: %u\n", (uint16_t)token->seq->elements[4]->uint);
    
    printf("Sender Hardware Address: ");
    print_bytes(token->seq->elements[5]->bytes.token, token->seq->elements[5]->bytes.len);
    
    printf("Sender Protocol Address: ");
    print_bytes(token->seq->elements[6]->bytes.token, token->seq->elements[6]->bytes.len);
    
    printf("Target Hardware Address: ");
    print_bytes(token->seq->elements[7]->bytes.token, token->seq->elements[7]->bytes.len);
    
    printf("Target Protocol Address: ");
    print_bytes(token->seq->elements[8]->bytes.token, token->seq->elements[8]->bytes.len);
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

    uint8_t *input = malloc(size);
    if(!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if(fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }

    HParser *parser = init_arp_parser();
    const HParsedToken *token = h_parse(parser, input, size);

    if(token) {
        print_arp_packet(token);
        h_parse_result_free(token);
    } else {
        fprintf(stderr, "Failed to parse ARP packet\n");
    }

    free(input);
    fclose(f);
    return 0;
}