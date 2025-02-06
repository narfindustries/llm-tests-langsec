#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    HParsedToken* htype;
    HParsedToken* ptype;
    HParsedToken* hlen;
    HParsedToken* plen;
    HParsedToken* oper;
    HParsedToken* sha;
    HParsedToken* spa;
    HParsedToken* tha;
    HParsedToken* tpa;
} ARPPacket;

HParser* arp_parser_create() {
    return h_sequence(
        h_uint16(),   // Hardware Type
        h_uint16(),   // Protocol Type
        h_uint8(),    // Hardware Address Length
        h_uint8(),    // Protocol Address Length
        h_uint16(),   // Operation
        h_repeat_n(h_uint8(), 6),  // Sender Hardware Address
        h_repeat_n(h_uint8(), 4),  // Sender Protocol Address
        h_repeat_n(h_uint8(), 6),  // Target Hardware Address
        h_repeat_n(h_uint8(), 4),  // Target Protocol Address
        NULL
    );
}

void print_arp_packet(ARPPacket* packet) {
    printf("Hardware Type: %d\n", (int)(intptr_t)packet->htype->token.uint);
    printf("Protocol Type: 0x%04x\n", (int)(intptr_t)packet->ptype->token.uint);
    printf("Hardware Address Length: %d\n", (int)(intptr_t)packet->hlen->token.uint);
    printf("Protocol Address Length: %d\n", (int)(intptr_t)packet->plen->token.uint);
    printf("Operation: %d\n", (int)(intptr_t)packet->oper->token.uint);

    printf("Sender Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x%s", (int)(intptr_t)packet->sha->token.sequence.elements[i]->token.uint, i < 5 ? ":" : "\n");
    }

    printf("Sender Protocol Address: ");
    for (int i = 0; i < 4; i++) {
        printf("%d%s", (int)(intptr_t)packet->spa->token.sequence.elements[i]->token.uint, i < 3 ? "." : "\n");
    }

    printf("Target Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x%s", (int)(intptr_t)packet->tha->token.sequence.elements[i]->token.uint, i < 5 ? ":" : "\n");
    }

    printf("Target Protocol Address: ");
    for (int i = 0; i < 4; i++) {
        printf("%d%s", (int)(intptr_t)packet->tpa->token.sequence.elements[i]->token.uint, i < 3 ? "." : "\n");
    }
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = arp_parser_create();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result) {
        ARPPacket* packet = (ARPPacket*)result->ast;
        print_arp_packet(packet);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }

    free(buffer);
    h_parser_free(parser);
    return 0;
}