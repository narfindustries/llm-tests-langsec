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
    uint8_t sender_hardware_addr[6]; 
    uint8_t sender_protocol_addr[4]; 
    uint8_t target_hardware_addr[6]; 
    uint8_t target_protocol_addr[4]; 

} arp_packet;

hammer_parser uint16_parser() {
    return h_uint16(seq(h_uint8(take(1)), h_uint8(take(1))));
}

hammer_parser uint8_parser() {
  return h_uint8(take(1));
}

hammer_parser arr_parser(hammer_parser p, size_t len) {
    return h_arr(p, len);
}


hammer_parser arp_parser() {
    return h_struct(arp_packet,
               seq(uint16_parser(), 
                   uint16_parser(), 
                   uint8_parser(), 
                   uint8_parser(), 
                   uint16_parser(), 
                   arr_parser(uint8_parser(), 6), 
                   arr_parser(uint8_parser(), 4), 
                   arr_parser(uint8_parser(), 6), 
                   arr_parser(uint8_parser(), 4)));
}



int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser parser = arp_parser();
    hammer_result result = hammer_parse(parser, buffer, fsize);

    if (result.status == HAMMER_SUCCESS) {
        arp_packet *packet = (arp_packet *)result.value;
        printf("Hardware Type: %u\n", packet->hardware_type);
        printf("Protocol Type: %u\n", packet->protocol_type);
        printf("Hardware Addr Len: %u\n", packet->hardware_addr_len);
        printf("Protocol Addr Len: %u\n", packet->protocol_addr_len);
        printf("Operation: %u\n", packet->operation);
        printf("Sender Hardware Addr: ");
        for (int i = 0; i < 6; i++) {
            printf("%02x:", packet->sender_hardware_addr[i]);
        }
        printf("\n");
        printf("Sender Protocol Addr: ");
        for (int i = 0; i < 4; i++) {
            printf("%02x:", packet->sender_protocol_addr[i]);
        }
        printf("\n");
        printf("Target Hardware Addr: ");
        for (int i = 0; i < 6; i++) {
            printf("%02x:", packet->target_hardware_addr[i]);
        }
        printf("\n");
        printf("Target Protocol Addr: ");
        for (int i = 0; i < 4; i++) {
            printf("%02x:", packet->target_protocol_addr[i]);
        }
        printf("\n");
        free(result.value);

    } else {
        fprintf(stderr, "Parsing failed: %s\n", result.error_message);
    }

    free(buffer);
    return 0;
}