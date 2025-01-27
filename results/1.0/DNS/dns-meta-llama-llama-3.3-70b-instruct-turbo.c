#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the structure for DNS packet
typedef struct {
    unsigned short id;
    unsigned short flags;
    unsigned short qdcount;
    unsigned short ancount;
    unsigned short nscount;
    unsigned short arcount;
    unsigned char *query;
    unsigned char *answer;
} DNS_packet;

// Function to parse the DNS packet
void parse_dns_packet(unsigned char *packet, int length) {
    DNS_packet *dns = (DNS_packet *) packet;

    // Extract the ID
    unsigned short id = ntohs(dns->id);

    // Extract the flags
    unsigned short flags = ntohs(dns->flags);

    // Extract the query count
    unsigned short qdcount = ntohs(dns->qdcount);

    // Extract the answer count
    unsigned short ancount = ntohs(dns->ancount);

    // Extract the query
    unsigned char *query = dns->query;

    // Extract the answer
    unsigned char *answer = dns->answer;

    // Process the query and answer
    printf("ID: %d\n", id);
    printf("Flags: %d\n", flags);
    printf("Query Count: %d\n", qdcount);
    printf("Answer Count: %d\n", ancount);
    printf("Query: %s\n", query);
    printf("Answer: %s\n", answer);
}

int main() {
    // Create a sample DNS packet
    unsigned char packet[] = {
        0x12, 0x34, // ID
        0x01, 0x00, // Flags
        0x00, 0x01, // Query Count
        0x00, 0x01, // Answer Count
        0x00, 0x00, // NS Count
        0x00, 0x00, // AR Count
        0x03, 0x77, 0x77, 0x77, 0x07, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x03, 0x63, 0x6f, 0x6d, 0x00, // Query
        0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 // Answer
    };

    // Parse the DNS packet
    parse_dns_packet(packet, sizeof(packet));

    return 0;
}