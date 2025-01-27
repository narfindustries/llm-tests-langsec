#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the DNS packet structure
typedef struct {
    unsigned short id;
    unsigned short flags;
    unsigned short qdcount;
    unsigned short ancount;
    unsigned short nscount;
    unsigned short arcount;
} DNSHeader;

typedef struct {
    unsigned short type;
    unsigned short class;
} DNSQuestion;

typedef struct {
    unsigned short type;
    unsigned short class;
    unsigned int ttl;
    unsigned short rdlength;
} DNSResource;

int main() {
    // Initialize the DNS packet
    DNSHeader header;
    header.id = 0x1234;
    header.flags = 0x0100;
    header.qdcount = 1;
    header.ancount = 0;
    header.nscount = 0;
    header.arcount = 0;

    DNSQuestion question;
    question.type = 1;
    question.class = 1;

    DNSResource resource;
    resource.type = 1;
    resource.class = 1;
    resource.ttl = 3600;
    resource.rdlength = 4;

    // Compile the DNS packet
    unsigned char packet[1024];
    unsigned int packet_len = 0;

    // Copy the header
    memcpy(packet + packet_len, &header, sizeof(header));
    packet_len += sizeof(header);

    // Copy the question
    memcpy(packet + packet_len, &question, sizeof(question));
    packet_len += sizeof(question);

    // Copy the resource
    memcpy(packet + packet_len, &resource, sizeof(resource));
    packet_len += sizeof(resource);

    // Add the IP address
    unsigned int ip = 0x01010101;
    memcpy(packet + packet_len, &ip, sizeof(ip));
    packet_len += sizeof(ip);

    // Output the DNS packet
    FILE *output = fopen("output", "wb");
    if (output == NULL) {
        printf("Failed to open output file\n");
        return 1;
    }

    fwrite(packet, 1, packet_len, output);
    fclose(output);

    return 0;
}