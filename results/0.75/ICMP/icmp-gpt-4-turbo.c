#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint32_t rest_of_header;
    uint8_t *data;
    size_t data_len;
} ICMP_Packet;

// Parser declarations
HParser *icmp_type;
HParser *icmp_code;
HParser *icmp_checksum;
HParser *icmp_rest_of_header;
HParser *icmp_data;
HParser *icmp_packet;

void init_parsers() {
    icmp_type = h_uint8();
    icmp_code = h_uint8();
    icmp_checksum = h_uint16();
    icmp_rest_of_header = h_uint32();
    icmp_data = h_bytes(1);

    icmp_packet = h_sequence(icmp_type,
                             icmp_code,
                             icmp_checksum,
                             icmp_rest_of_header,
                             h_indirect(h_length_value(h_uint32(), h_greedy_bytes())),
                             NULL);
}

ICMP_Packet *parse_icmp(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(icmp_packet, data, length);
    if (result == NULL) {
        return NULL;
    }

    HCountedArray *parsed_packet = (HCountedArray *)result->ast;
    ICMP_Packet *packet = malloc(sizeof(ICMP_Packet));
    if (!packet) {
        h_parse_result_free(result);
        return NULL;
    }

    packet->type = ((uint8_t*)parsed_packet->elements[0])->uint8;
    packet->code = ((uint8_t*)parsed_packet->elements[1])->uint8;
    packet->checksum = ((uint16_t*)parsed_packet->elements[2])->uint16;
    packet->rest_of_header = ((uint32_t*)parsed_packet->elements[3])->uint32;
    HBytes *data_field = (HBytes *)parsed_packet->elements[4];
    packet->data_len = data_field->len;
    packet->data = malloc(data_field->len);
    if (!packet->data) {
        free(packet);
        h_parse_result_free(result);
        return NULL;
    }
    memcpy(packet->data, data_field->token, data_field->len);

    h_parse_result_free(result);
    return packet;
}

void free_icmp_packet(ICMP_Packet *packet) {
    if (packet) {
        free(packet->data);
        free(packet);
    }
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ICMP Binary File>\n", argv[0]);
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
    uint8_t *data = malloc(length);
    if (!data) {
        fclose(file);
        fprintf(stderr, "Failed to allocate memory for reading file\n");
        return 1;
    }

    if (fread(data, 1, length, file) != length) {
        fclose(file);
        free(data);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }

    fclose(file);

    init_parsers();

    ICMP_Packet *packet = parse_icmp(data, length);
    free(data);

    if (!packet) {
        fprintf(stderr, "Failed to parse ICMP packet\n");
        return 1;
    }

    printf("ICMP Packet Parsed:\n");
    printf("Type: %u\n", packet->type);
    printf("Code: %u\n", packet->code);
    printf("Checksum: %u\n", packet->checksum);
    printf("Rest of Header: %u\n", packet->rest_of_header);
    printf("Data Length: %zu\n", packet->data_len);

    free_icmp_packet(packet);

    return 0;
}