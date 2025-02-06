#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t op;
    uint8_t sha[6];
    uint32_t spa;
    uint8_t tha[6];
    uint32_t tpa;
} arp_packet;

#define DEFINE_PARSER(parser_name) \
    int parser_name(const uint8_t *data, size_t len) {

#define TRY1(code) code
#define TRY2(code1, code2) code1; code2
#define TRY3(code1, code2, code3) code1; code2; code3
#define TRY4(code1, code2, code3, code4) code1; code2; code3; code4
#define TRY5(code1, code2, code3, code4, code5) code1; code2; code3; code4; code5
#define TRY6(code1, code2, code3, code4, code5, code6) code1; code2; code3; code4; code5; code6
#define TRY7(code1, code2, code3, code4, code5, code6, code7) code1; code2; code3; code4; code5; code6; code7
#define TRY8(code1, code2, code3, code4, code5, code6, code7, code8) code1; code2; code3; code4; code5; code6; code7; code8

#define UINT16(var) \
    if (len < 2) return 0; \
    var = (uint16_t)data[0] << 8 | data[1]; \
    data += 2; \
    len -= 2;

#define UINT8(var) \
    if (len < 1) return 0; \
    var = data[0]; \
    data += 1; \
    len -= 1;

#define ARRAY(var, len) \
    if (len < len) return 0; \
    var = (uint8_t *)malloc(len); \
    memcpy(var, data, len); \
    data += len; \
    len -= len;

DEFINE_PARSER(arp_parser)
    uint16_t htype;
    uint16_t ptype;
    uint8_t hlen;
    uint8_t plen;
    uint16_t op;
    uint8_t *sha;
    uint8_t *spa_bytes;
    uint8_t *tha;
    uint8_t *tpa_bytes;

    TRY8(
        UINT16(htype),
        UINT16(ptype),
        UINT8(hlen),
        UINT8(plen),
        UINT16(op),
        ARRAY(sha, hlen),
        ARRAY(spa_bytes, plen),
        ARRAY(tha, hlen)
    );
    ARRAY(tpa_bytes, plen);

    uint32_t spa = 0;
    for (int i = 0; i < plen; i++) {
        spa |= (uint32_t)spa_bytes[i] << (8 * (plen - 1 - i));
    }

    uint32_t tpa = 0;
    for (int i = 0; i < plen; i++) {
        tpa |= (uint32_t)tpa_bytes[i] << (8 * (plen - 1 - i));
    }

    arp_packet packet;
    packet.htype = htype;
    packet.ptype = ptype;
    packet.hlen = hlen;
    packet.plen = plen;
    packet.op = op;
    memcpy(packet.sha, sha, hlen);
    packet.spa = spa;
    memcpy(packet.tha, tha, hlen);
    packet.tpa = tpa;

    printf("ARP Packet:\n");
    printf("  Hardware Type: 0x%04x\n", packet.htype);
    printf("  Protocol Type: 0x%04x\n", packet.ptype);
    printf("  Hardware Address Length: %d\n", packet.hlen);
    printf("  Protocol Address Length: %d\n", packet.plen);
    printf("  Operation: 0x%04x\n", packet.op);
    printf("  Sender Hardware Address: ");
    for (int i = 0; i < packet.hlen; i++) {
        printf("%02x", packet.sha[i]);
    }
    printf("\n");
    printf("  Sender Protocol Address: 0x%08x\n", packet.spa);
    printf("  Target Hardware Address: ");
    for (int i = 0; i < packet.hlen; i++) {
        printf("%02x", packet.tha[i]);
    }
    printf("\n");
    printf("  Target Protocol Address: 0x%08x\n", packet.tpa);

    free(sha);
    free(spa_bytes);
    free(tha);
    free(tpa_bytes);

    return 1;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Could not open file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Could not allocate memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Could not read file %s\n", argv[1]);
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    arp_parser(data, file_size);

    free(data);
    return 0;
}