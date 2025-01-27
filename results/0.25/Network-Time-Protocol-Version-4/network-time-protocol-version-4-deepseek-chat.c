#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the NTP packet structure
typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_packet_t;

// Define the Hammer parser for the NTP packet
HParser *ntp_parser() {
    return h_sequence(
        h_uint8(),  // li_vn_mode
        h_uint8(),  // stratum
        h_uint8(),  // poll
        h_uint8(),  // precision
        h_uint32(), // root_delay
        h_uint32(), // root_dispersion
        h_uint32(), // reference_id
        h_uint64(), // reference_timestamp
        h_uint64(), // originate_timestamp
        h_uint64(), // receive_timestamp
        h_uint64(), // transmit_timestamp
        NULL
    );
}

// Function to parse the NTP packet
ntp_packet_t *parse_ntp_packet(const uint8_t *data, size_t data_len) {
    HParser *parser = ntp_parser();
    HParseResult *result = h_parse(parser, data, data_len);

    if (!result) {
        fprintf(stderr, "Failed to parse NTP packet\n");
        return NULL;
    }

    ntp_packet_t *packet = (ntp_packet_t *)malloc(sizeof(ntp_packet_t));
    if (!packet) {
        fprintf(stderr, "Memory allocation failed\n");
        h_parse_result_free(result);
        return NULL;
    }

    packet->li_vn_mode = *(uint8_t *)result->ast->seq->elements[0]->token;
    packet->stratum = *(uint8_t *)result->ast->seq->elements[1]->token;
    packet->poll = *(uint8_t *)result->ast->seq->elements[2]->token;
    packet->precision = *(uint8_t *)result->ast->seq->elements[3]->token;
    packet->root_delay = *(uint32_t *)result->ast->seq->elements[4]->token;
    packet->root_dispersion = *(uint32_t *)result->ast->seq->elements[5]->token;
    packet->reference_id = *(uint32_t *)result->ast->seq->elements[6]->token;
    packet->reference_timestamp = *(uint64_t *)result->ast->seq->elements[7]->token;
    packet->originate_timestamp = *(uint64_t *)result->ast->seq->elements[8]->token;
    packet->receive_timestamp = *(uint64_t *)result->ast->seq->elements[9]->token;
    packet->transmit_timestamp = *(uint64_t *)result->ast->seq->elements[10]->token;

    h_parse_result_free(result);
    return packet;
}

// Function to free the NTP packet
void free_ntp_packet(ntp_packet_t *packet) {
    if (packet) {
        free(packet);
    }
}

// Example usage
int main() {
    uint8_t data[] = {
        0x1C, 0x02, 0x06, 0xEC, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };
    size_t data_len = sizeof(data);

    ntp_packet_t *packet = parse_ntp_packet(data, data_len);
    if (packet) {
        printf("Parsed NTP packet successfully\n");
        free_ntp_packet(packet);
    }

    return 0;
}