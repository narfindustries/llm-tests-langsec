#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Define the DNS packet structure
typedef struct {
    uint16_t id;
    uint16_t flags;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} dns_header_t;

typedef struct {
    uint16_t type;
    uint16_t class;
} dns_question_t;

typedef struct {
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
} dns_rr_t;

// Define the Hammer specification
int main(int argc, char** argv) {
    // Open the input file
    FILE* input_file = fopen("input.bin", "rb");
    if (!input_file) {
        return 1;
    }

    // Read the DNS packet
    dns_header_t header;
    fread(&header, sizeof(header), 1, input_file);

    // Check if the packet is a query
    if (header.flags & 0x8000) {
        // Process the query
        dns_question_t question;
        fread(&question, sizeof(question), 1, input_file);

        // Check the query type
        if (question.type == 1) {
            // Process the A record query
            dns_rr_t answer;
            fread(&answer, sizeof(answer), 1, input_file);

            // Get the answer IP address
            uint32_t ip_address;
            fread(&ip_address, sizeof(ip_address), 1, input_file);

            // Print the answer IP address
            printf("%08x\n", ip_address);
        }
    }

    // Close the input file
    fclose(input_file);

    return 0;
}