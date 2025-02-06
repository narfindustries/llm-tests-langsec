#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser for ARP packet
HParser* create_arp_parser(void) {
    // Fixed-size fields
    HParser* hardware_type = h_uint16();
    HParser* protocol_type = h_uint16();
    HParser* hardware_addr_len = h_uint8();
    HParser* protocol_addr_len = h_uint8();
    HParser* operation = h_uint16();

    // Variable-length fields based on HLEN and PLEN
    // We'll use indirect parser to handle dynamic lengths
    HParser* sender_hardware_addr = h_repeat_n(h_uint8(), 6);  // Most common: MAC address (6 bytes)
    HParser* sender_protocol_addr = h_repeat_n(h_uint8(), 4);  // Most common: IPv4 (4 bytes)
    HParser* target_hardware_addr = h_repeat_n(h_uint8(), 6);  // Most common: MAC address (6 bytes)
    HParser* target_protocol_addr = h_repeat_n(h_uint8(), 4);  // Most common: IPv4 (4 bytes)

    // Combine all fields in sequence
    return h_sequence(hardware_type,
                     protocol_type,
                     hardware_addr_len,
                     protocol_addr_len,
                     operation,
                     sender_hardware_addr,
                     sender_protocol_addr,
                     target_hardware_addr,
                     target_protocol_addr,
                     NULL);
}

void print_bytes(uint8_t* bytes, size_t len) {
    for (size_t i = 0; i < len; i++) {
        printf("%02x", bytes[i]);
        if (i < len - 1) printf(":");
    }
}

void print_parse_result(HParsedToken* result) {
    if (!result) return;

    HCountedArray* seq = result->seq;
    if (!seq) return;

    // Extract fields
    uint16_t hardware_type = seq->elements[0]->uint;
    uint16_t protocol_type = seq->elements[1]->uint;
    uint8_t hardware_addr_len = seq->elements[2]->uint;
    uint8_t protocol_addr_len = seq->elements[3]->uint;
    uint16_t operation = seq->elements[4]->uint;

    // Print fixed fields
    printf("Hardware Type: 0x%04x\n", hardware_type);
    printf("Protocol Type: 0x%04x\n", protocol_type);
    printf("Hardware Address Length: %u\n", hardware_addr_len);
    printf("Protocol Address Length: %u\n", protocol_addr_len);
    printf("Operation: %u (%s)\n", operation, operation == 1 ? "REQUEST" : "REPLY");

    // Print variable length fields
    printf("Sender Hardware Address: ");
    print_bytes((uint8_t*)seq->elements[5]->seq->elements, 6);
    printf("\n");

    printf("Sender Protocol Address: ");
    print_bytes((uint8_t*)seq->elements[6]->seq->elements, 4);
    printf("\n");

    printf("Target Hardware Address: ");
    print_bytes((uint8_t*)seq->elements[7]->seq->elements, 6);
    printf("\n");

    printf("Target Protocol Address: ");
    print_bytes((uint8_t*)seq->elements[8]->seq->elements, 4);
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
        return 1;
    }

    // Read input file
    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        fclose(f);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        free(input);
        fclose(f);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }
    fclose(f);

    // Create and run parser
    HParser *parser = create_arp_parser();
    if (!parser) {
        free(input);
        fprintf(stderr, "Failed to create parser\n");
        return 1;
    }

    HParseResult *result = h_parse(parser, input, size);
    if (!result) {
        free(input);
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    // Print results
    print_parse_result(result->ast);

    // Cleanup
    h_parse_result_free(result);
    free(input);
    return 0;
}