#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_addr_len;
    uint8_t protocol_addr_len;
    uint16_t operation;
    HBytes sender_hardware_addr;
    HBytes sender_protocol_addr;
    HBytes target_hardware_addr;
    HBytes target_protocol_addr;
} ARPPacket;

HParser* arp_parser(void) {
    return h_sequence(
        h_uint16(),   // hardware_type
        h_uint16(),   // protocol_type
        h_uint8(),    // hardware_addr_len
        h_uint8(),    // protocol_addr_len
        h_uint16(),   // operation
        h_repeat_n(h_uint8(), 6),  // sender_hardware_addr
        h_repeat_n(h_uint8(), 4),  // sender_protocol_addr
        h_repeat_n(h_uint8(), 6),  // target_hardware_addr
        h_repeat_n(h_uint8(), 4),  // target_protocol_addr
        NULL
    );
}

void print_arp_packet(ARPPacket* packet) {
    printf("Hardware Type: %d\n", packet->hardware_type);
    printf("Protocol Type: 0x%04x\n", packet->protocol_type);
    printf("Hardware Address Length: %d\n", packet->hardware_addr_len);
    printf("Protocol Address Length: %d\n", packet->protocol_addr_len);
    printf("Operation: %d\n", packet->operation);

    printf("Sender Hardware Address: ");
    for(size_t i = 0; i < packet->sender_hardware_addr.len; i++) {
        printf("%02x%s", packet->sender_hardware_addr.token[i], 
               i < packet->sender_hardware_addr.len - 1 ? ":" : "\n");
    }

    printf("Sender Protocol Address: ");
    for(size_t i = 0; i < packet->sender_protocol_addr.len; i++) {
        printf("%d%s", packet->sender_protocol_addr.token[i], 
               i < packet->sender_protocol_addr.len - 1 ? "." : "\n");
    }

    printf("Target Hardware Address: ");
    for(size_t i = 0; i < packet->target_hardware_addr.len; i++) {
        printf("%02x%s", packet->target_hardware_addr.token[i], 
               i < packet->target_hardware_addr.len - 1 ? ":" : "\n");
    }

    printf("Target Protocol Address: ");
    for(size_t i = 0; i < packet->target_protocol_addr.len; i++) {
        printf("%d%s", packet->target_protocol_addr.token[i], 
               i < packet->target_protocol_addr.len - 1 ? "." : "\n");
    }
}

int main(int argc, char* argv[]) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if(!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if(!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if(fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = arp_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if(result && result->ast) {
        ARPPacket packet;
        HCountedArray* arr = result->ast;
        
        packet.hardware_type = *(uint16_t*)arr->elements[0];
        packet.protocol_type = *(uint16_t*)arr->elements[1];
        packet.hardware_addr_len = *(uint8_t*)arr->elements[2];
        packet.protocol_addr_len = *(uint8_t*)arr->elements[3];
        packet.operation = *(uint16_t*)arr->elements[4];
        
        packet.sender_hardware_addr = *(HBytes*)arr->elements[5];
        packet.sender_protocol_addr = *(HBytes*)arr->elements[6];
        packet.target_hardware_addr = *(HBytes*)arr->elements[7];
        packet.target_protocol_addr = *(HBytes*)arr->elements[8];

        print_arp_packet(&packet);
    } else {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);
    return 0;
}