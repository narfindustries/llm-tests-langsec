#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function prototypes
void parse_ntp_packet(const uint8_t *data, size_t length);
void print_ntp_packet(const HParsedToken *pkt);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_ntp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    if (read_size != file_size) {
        fprintf(stderr, "Error reading file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    parse_ntp_packet(buffer, file_size);

    free(buffer);
    fclose(file);
    return 0;
}

void parse_ntp_packet(const uint8_t *data, size_t length) {
    HParser *ntp_parser = h_sequence(
        h_bits(2, false), // Leap Indicator
        h_bits(3, false), // Version Number
        h_bits(3, false), // Mode
        h_uint8(),        // Stratum
        h_int8(),         // Poll
        h_int8(),         // Precision
        h_int32(),        // Root Delay
        h_int32(),        // Root Dispersion
        h_bits(32, false),// Reference Identifier
        h_int64(),        // Reference Timestamp
        h_int64(),        // Origin Timestamp
        h_int64(),        // Receive Timestamp
        h_int64(),        // Transmit Timestamp
        h_many(h_length_value(h_uint16(), h_uint8())), // Extension fields
        NULL
    );

    HParseResult *result = h_parse(ntp_parser, data, length);
    if (result) {
        print_ntp_packet(result->ast);
    } else {
        fprintf(stderr, "Failed to parse NTP packet\n");
    }

    h_parse_result_free(result);
    h_parser_destroy(ntp_parser); // Correct usage of parser destruction
}

void print_ntp_packet(const HParsedToken *pkt) {
    printf("Leap Indicator: %llu\n", pkt->seq->elements[0]->uint);
    printf("Version: %llu\n", pkt->seq->elements[1]->uint);
    printf("Mode: %llu\n", pkt->seq->elements[2]->uint);
    printf("Stratum: %u\n", pkt->seq->elements[3]->uint);
    printf("Poll: %d\n", pkt->seq->elements[4]->sint);
    printf("Precision: %d\n", pkt->seq->elements[5]->sint);
    printf("Root Delay: %d\n", pkt->seq->elements[6]->sint);
    printf("Root Dispersion: %d\n", pkt->seq->elements[7]->sint);
    printf("Reference Identifier: %llu\n", pkt->seq->elements[8]->uint);
    printf("Reference Timestamp: %lld\n", pkt->seq->elements[9]->sint);
    printf("Origin Timestamp: %lld\n", pkt->seq->elements[10]->sint);
    printf("Receive Timestamp: %lld\n", pkt->seq->elements[11]->sint);
    printf("Transmit Timestamp: %lld\n", pkt->seq->elements[12]->sint);

    // Print extension fields if any
    const HParsedToken *ext_fields = pkt->seq->elements[13];
    for (size_t i = 0; i < ext_fields->seq->used; i++) {
        printf("Extension Field %zu: ", i);
        for (size_t j = 0; j < ext_fields->seq->elements[i]->seq->used; j++) {
            printf("%02x ", ext_fields->seq->elements[i]->seq->elements[j]->uint);
        }
        printf("\n");
    }
}