#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define ARP_ETHERNET 1
#define ARP_IPV4 0x0800
#define ARP_REQUEST 1
#define ARP_REPLY 2

typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t oper;
    uint8_t sha[6];
    uint32_t spa;
    uint8_t tha[6];
    uint32_t tpa;
} arp_packet_t;

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("fopen");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("malloc");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        perror("fread");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    HParser *arp_packet = h_sequence(
        h_bind_uint16("htype"),
        h_bind_uint16("ptype"),
        h_bind_uint8("hlen"),
        h_bind_uint8("plen"),
        h_bind_uint16("oper"),
        h_bind_array("sha", 6, h_bind_uint8(NULL)),
        h_bind_uint32("spa"),
        h_bind_array("tha", 6, h_bind_uint8(NULL)),
        h_bind_uint32("tpa")
    );

    arp_packet_t packet;
    HParseResult *result = h_parse(arp_packet, data, file_size);
    if (result == NULL) {
        printf("Parse error\n");
        free(data);
        return 1;
    }

    printf("Hardware Type: %u\n", packet.htype);
    printf("Protocol Type: 0x%04x\n", packet.ptype);
    printf("Hardware Address Length: %u\n", packet.hlen);
    printf("Protocol Address Length: %u\n", packet.plen);
    printf("Operation: %u\n", packet.oper);
    printf("Sender Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", packet.sha[i]);
    }
    printf("\n");
    printf("Sender Protocol Address: 0x%08x\n", packet.spa);
    printf("Target Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", packet.tha[i]);
    }
    printf("\n");
    printf("Target Protocol Address: 0x%08x\n", packet.tpa);

    free(data);
    return 0;
}