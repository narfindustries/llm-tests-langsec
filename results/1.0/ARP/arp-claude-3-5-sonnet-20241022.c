#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ARP Parser definition
HParser* create_arp_parser(void) {
    // Hardware Type (16 bits)
    HParser* hardware_type = h_uint16();

    // Protocol Type (16 bits)
    HParser* protocol_type = h_uint16();

    // Hardware Address Length (8 bits)
    HParser* hardware_len = h_uint8();

    // Protocol Address Length (8 bits)
    HParser* protocol_len = h_uint8();

    // Operation Code (16 bits)
    HParser* operation = h_uint16();

    // Variable length fields based on hardware_len and protocol_len
    HParser* sender_hardware_addr = h_length_value(hardware_len, h_uint8());
    HParser* sender_protocol_addr = h_length_value(protocol_len, h_uint8());
    HParser* target_hardware_addr = h_length_value(hardware_len, h_uint8());
    HParser* target_protocol_addr = h_length_value(protocol_len, h_uint8());

    // Combine all fields in sequence
    return h_sequence(hardware_type,
                     protocol_type,
                     hardware_len,
                     protocol_len,
                     operation,
                     sender_hardware_addr,
                     sender_protocol_addr,
                     target_hardware_addr,
                     target_protocol_addr,
                     NULL);
}

void print_bytes(uint8_t* data, size_t len) {
    for(size_t i = 0; i < len; i++) {
        printf("%02x", data[i]);
    }
    printf("\n");
}

int main(int argc, char** argv) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE* f = fopen(argv[1], "rb");
    if(!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if(fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        fclose(f);
        free(input);
        return 1;
    }
    fclose(f);

    // Create and run parser
    HParser* arp_parser = create_arp_parser();
    HParseResult* result = h_parse(arp_parser, input, size);

    if(!result) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        return 1;
    }

    // Extract and print results
    HParsedToken* token = result->ast;
    if(token->token_type == TT_SEQUENCE) {
        printf("Hardware Type: 0x%04x\n", 
               (unsigned int)token->seq->elements[0]->uint);
        printf("Protocol Type: 0x%04x\n", 
               (unsigned int)token->seq->elements[1]->uint);
        printf("Hardware Address Length: %u\n", 
               (unsigned int)token->seq->elements[2]->uint);
        printf("Protocol Address Length: %u\n", 
               (unsigned int)token->seq->elements[3]->uint);
        printf("Operation: %u\n", 
               (unsigned int)token->seq->elements[4]->uint);
        
        printf("Sender Hardware Address: ");
        print_bytes(token->seq->elements[5]->bytes.token, 
                   token->seq->elements[5]->bytes.len);
        
        printf("Sender Protocol Address: ");
        print_bytes(token->seq->elements[6]->bytes.token, 
                   token->seq->elements[6]->bytes.len);
        
        printf("Target Hardware Address: ");
        print_bytes(token->seq->elements[7]->bytes.token, 
                   token->seq->elements[7]->bytes.len);
        
        printf("Target Protocol Address: ");
        print_bytes(token->seq->elements[8]->bytes.token, 
                   token->seq->elements[8]->bytes.len);
    }

    h_parse_result_free(result);
    free(input);
    return 0;
}