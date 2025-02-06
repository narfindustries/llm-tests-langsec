#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the ICMP header structure
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
    uint32_t gateway_internet_address;
    uint8_t pointer;
    uint32_t originate_timestamp;
    uint32_t receive_timestamp;
    uint32_t transmit_timestamp;
    uint32_t address_mask;
    uint8_t *data;
    size_t data_length;
} ICMPHeader;

// Parser for ICMP header
HParser *icmp_parser() {
    return h_sequence(
        h_uint8(), // type
        h_uint8(), // code
        h_uint16(), // checksum
        h_uint16(), // identifier
        h_uint16(), // sequence_number
        h_optional(h_uint32()), // gateway_internet_address
        h_optional(h_uint8()), // pointer
        h_optional(h_uint32()), // originate_timestamp
        h_optional(h_uint32()), // receive_timestamp
        h_optional(h_uint32()), // transmit_timestamp
        h_optional(h_uint32()), // address_mask
        h_length_value(h_uint16(), h_uint8()), // data
        NULL
    );
}

// Main function
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = icmp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ICMP header\n");
        free(buffer);
        return 1;
    }

    ICMPHeader *header = (ICMPHeader *)result->ast;
    printf("Type: %u\n", header->type);
    printf("Code: %u\n", header->code);
    printf("Checksum: %u\n", header->checksum);
    printf("Identifier: %u\n", header->identifier);
    printf("Sequence Number: %u\n", header->sequence_number);
    if (header->gateway_internet_address)
        printf("Gateway Internet Address: %u\n", header->gateway_internet_address);
    if (header->pointer)
        printf("Pointer: %u\n", header->pointer);
    if (header->originate_timestamp)
        printf("Originate Timestamp: %u\n", header->originate_timestamp);
    if (header->receive_timestamp)
        printf("Receive Timestamp: %u\n", header->receive_timestamp);
    if (header->transmit_timestamp)
        printf("Transmit Timestamp: %u\n", header->transmit_timestamp);
    if (header->address_mask)
        printf("Address Mask: %u\n", header->address_mask);
    if (header->data_length > 0) {
        printf("Data: ");
        for (size_t i = 0; i < header->data_length; i++) {
            printf("%02x ", header->data[i]);
        }
        printf("\n");
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}