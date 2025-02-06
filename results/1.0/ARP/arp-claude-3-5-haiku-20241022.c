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
    uint8_t *sender_hw_addr;
    uint8_t *sender_protocol_addr;
    uint8_t *target_hw_addr;
    uint8_t *target_protocol_addr;
} ARPPacket;

static HParser *arp_parser = NULL;

static HParsedToken* parse_arp_packet(const HParseResult *result, void *user_data) {
    ARPPacket *packet = calloc(1, sizeof(ARPPacket));
    const HParsedToken *token = result->ast;

    packet->hardware_type = token[0].uint;
    packet->protocol_type = token[1].uint;
    packet->hardware_addr_len = token[2].uint;
    packet->protocol_addr_len = token[3].uint;
    packet->operation = token[4].uint;

    packet->sender_hw_addr = malloc(packet->hardware_addr_len);
    packet->sender_protocol_addr = malloc(packet->protocol_addr_len);
    packet->target_hw_addr = malloc(packet->hardware_addr_len);
    packet->target_protocol_addr = malloc(packet->protocol_addr_len);

    memcpy(packet->sender_hw_addr, token[5].bytes.token, packet->hardware_addr_len);
    memcpy(packet->sender_protocol_addr, token[6].bytes.token, packet->protocol_addr_len);
    memcpy(packet->target_hw_addr, token[7].bytes.token, packet->hardware_addr_len);
    memcpy(packet->target_protocol_addr, token[8].bytes.token, packet->protocol_addr_len);

    HParsedToken *result_token = malloc(sizeof(HParsedToken));
    result_token->user = packet;
    return result_token;
}

static void init_arp_parser() {
    HParser *hardware_type = h_uint16();
    HParser *protocol_type = h_uint16();
    HParser *hardware_addr_len = h_uint8();
    HParser *protocol_addr_len = h_uint8();
    HParser *operation = h_uint16();
    
    arp_parser = h_action(
        h_sequence(
            hardware_type,
            protocol_type,
            hardware_addr_len,
            protocol_addr_len,
            operation,
            h_repeat_n(h_uint8(), (size_t)hardware_addr_len),   // sender hardware address
            h_repeat_n(h_uint8(), (size_t)protocol_addr_len),   // sender protocol address
            h_repeat_n(h_uint8(), (size_t)hardware_addr_len),   // target hardware address
            h_repeat_n(h_uint8(), (size_t)protocol_addr_len),   // target protocol address
            NULL
        ), parse_arp_packet, NULL);
}

void print_byte_array(uint8_t *arr, size_t len) {
    for (size_t i = 0; i < len; i++) {
        printf("%02x ", arr[i]);
    }
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    init_arp_parser();

    HParseResult *result = h_parse(arp_parser, buffer, file_size);
    if (result && result->ast) {
        ARPPacket *packet = result->ast->user;
        printf("ARP Packet Details:\n");
        printf("Hardware Type: %u\n", packet->hardware_type);
        printf("Protocol Type: %u\n", packet->protocol_type);
        printf("Hardware Address Length: %u\n", packet->hardware_addr_len);
        printf("Protocol Address Length: %u\n", packet->protocol_addr_len);
        printf("Operation: %u\n", packet->operation);
        
        printf("Sender Hardware Address: ");
        print_byte_array(packet->sender_hw_addr, packet->hardware_addr_len);
        
        printf("Sender Protocol Address: ");
        print_byte_array(packet->sender_protocol_addr, packet->protocol_addr_len);
        
        printf("Target Hardware Address: ");
        print_byte_array(packet->target_hw_addr, packet->hardware_addr_len);
        
        printf("Target Protocol Address: ");
        print_byte_array(packet->target_protocol_addr, packet->protocol_addr_len);

        // Free dynamically allocated memory
        free(packet->sender_hw_addr);
        free(packet->sender_protocol_addr);
        free(packet->target_hw_addr);
        free(packet->target_protocol_addr);
        free(packet);
    } else {
        printf("Parsing failed\n");
    }

    free(buffer);
    h_parse_result_free(result);
    return 0;
}