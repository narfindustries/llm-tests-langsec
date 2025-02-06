#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    union {
        struct {
            uint16_t identifier;
            uint16_t sequence_number;
        } echo;
        uint32_t gateway_address;
        uint8_t pointer;
        uint32_t unused;
        struct {
            uint16_t identifier;
            uint16_t sequence_number;
            uint32_t originate_timestamp;
            uint32_t receive_timestamp;
            uint32_t transmit_timestamp;
        } timestamp;
        struct {
            uint16_t identifier;
            uint16_t sequence_number;
            uint32_t address_mask;
        } address_mask;
    } rest_of_header;
    uint8_t *data;
    size_t data_length;
} ICMPPacket;

HParser *icmp_parser() {
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();
    HParser *echo = h_sequence(h_uint16(), h_uint16(), NULL);
    HParser *gateway_address = h_uint32();
    HParser *pointer = h_sequence(h_uint8(), h_uint32(), NULL);
    HParser *timestamp = h_sequence(h_uint16(), h_uint16(), h_uint32(), h_uint32(), h_uint32(), NULL);
    HParser *address_mask = h_sequence(h_uint16(), h_uint16(), h_uint32(), NULL);
    HParser *unused = h_uint32();
    HParser *rest_of_header = h_choice(echo, gateway_address, pointer, timestamp, address_mask, unused, NULL);
    HParser *data = h_many(h_uint8());
    return h_sequence(type, code, checksum, rest_of_header, data, NULL);
}

void parse_icmp_packet(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(icmp_parser(), data, length);
    if (result) {
        ICMPPacket packet;
        packet.type = *(uint8_t *)result->ast->token;
        packet.code = *(uint8_t *)result->ast->next->token;
        packet.checksum = *(uint16_t *)result->ast->next->next->token;
        HParseResult *rest = result->ast->next->next->next;
        switch (packet.type) {
            case 0:
            case 8:
                packet.rest_of_header.echo.identifier = *(uint16_t *)rest->token;
                packet.rest_of_header.echo.sequence_number = *(uint16_t *)rest->next->token;
                break;
            case 3:
            case 4:
            case 11:
                packet.rest_of_header.unused = *(uint32_t *)rest->token;
                break;
            case 5:
                packet.rest_of_header.gateway_address = *(uint32_t *)rest->token;
                break;
            case 12:
                packet.rest_of_header.pointer = *(uint8_t *)rest->token;
                break;
            case 13:
            case 14:
                packet.rest_of_header.timestamp.identifier = *(uint16_t *)rest->token;
                packet.rest_of_header.timestamp.sequence_number = *(uint16_t *)rest->next->token;
                packet.rest_of_header.timestamp.originate_timestamp = *(uint32_t *)rest->next->next->token;
                packet.rest_of_header.timestamp.receive_timestamp = *(uint32_t *)rest->next->next->next->token;
                packet.rest_of_header.timestamp.transmit_timestamp = *(uint32_t *)rest->next->next->next->next->token;
                break;
            case 17:
            case 18:
                packet.rest_of_header.address_mask.identifier = *(uint16_t *)rest->token;
                packet.rest_of_header.address_mask.sequence_number = *(uint16_t *)rest->next->token;
                packet.rest_of_header.address_mask.address_mask = *(uint32_t *)rest->next->next->token;
                break;
            default:
                break;
        }
        packet.data_length = result->ast->next->next->next->next->token_len;
        packet.data = (uint8_t *)result->ast->next->next->next->next->token;
        printf("Type: %d\n", packet.type);
        printf("Code: %d\n", packet.code);
        printf("Checksum: %d\n", packet.checksum);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ICMP packet.\n");
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }
    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *data = (uint8_t *)malloc(length);
    fread(data, 1, length, file);
    fclose(file);
    parse_icmp_packet(data, length);
    free(data);
    return 0;
}