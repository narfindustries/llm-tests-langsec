#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the NTPv4 packet structure
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

// Define the Hammer parser for the NTPv4 packet
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

// Function to parse the NTPv4 packet
ntp_packet_t *parse_ntp_packet(const uint8_t *data, size_t len) {
    HParser *parser = ntp_parser();
    HParseResult *result = h_parse(parser, data, len);
    if (!result) {
        fprintf(stderr, "Failed to parse NTP packet\n");
        return NULL;
    }

    ntp_packet_t *packet = malloc(sizeof(ntp_packet_t));
    if (!packet) {
        fprintf(stderr, "Failed to allocate memory for NTP packet\n");
        h_parse_result_free(result);
        return NULL;
    }

    packet->li_vn_mode = result->ast->seq->elements[0]->uint8;
    packet->stratum = result->ast->seq->elements[1]->uint8;
    packet->poll = result->ast->seq->elements[2]->uint8;
    packet->precision = result->ast->seq->elements[3]->uint8;
    packet->root_delay = result->ast->seq->elements[4]->uint32;
    packet->root_dispersion = result->ast->seq->elements[5]->uint32;
    packet->reference_id = result->ast->seq->elements[6]->uint32;
    packet->reference_timestamp = result->ast->seq->elements[7]->uint64;
    packet->originate_timestamp = result->ast->seq->elements[8]->uint64;
    packet->receive_timestamp = result->ast->seq->elements[9]->uint64;
    packet->transmit_timestamp = result->ast->seq->elements[10]->uint64;

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
        0x24, 0x01, 0x06, 0xE8, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };
    size_t len = sizeof(data);

    ntp_packet_t *packet = parse_ntp_packet(data, len);
    if (packet) {
        printf("Parsed NTP packet:\n");
        printf("LI_VN_MODE: 0x%02X\n", packet->li_vn_mode);
        printf("Stratum: 0x%02X\n", packet->stratum);
        printf("Poll: 0x%02X\n", packet->poll);
        printf("Precision: 0x%02X\n", packet->precision);
        printf("Root Delay: 0x%08X\n", packet->root_delay);
        printf("Root Dispersion: 0x%08X\n", packet->root_dispersion);
        printf("Reference ID: 0x%08X\n", packet->reference_id);
        printf("Reference Timestamp: 0x%016lX\n", packet->reference_timestamp);
        printf("Originate Timestamp: 0x%016lX\n", packet->originate_timestamp);
        printf("Receive Timestamp: 0x%016lX\n", packet->receive_timestamp);
        printf("Transmit Timestamp: 0x%016lX\n", packet->transmit_timestamp);

        free_ntp_packet(packet);
    }

    return 0;
}