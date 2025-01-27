#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Define structure for DNS header
typedef struct {
    uint16_t id;
    uint16_t flags;
    uint16_t qdcount;
    uint16_t ancount;
    uint16_t nscount;
    uint16_t arcount;
} dns_header_t;

// Define structure for DNS question
typedef struct {
    uint16_t qtype;
    uint16_t qclass;
} dns_question_t;

// Define structure for DNS answer
typedef struct {
    uint16_t type;
    uint16_t class;
    uint32_t ttl;
    uint16_t rdlength;
} dns_answer_t;

// Define structure for DNS packet
typedef struct {
    dns_header_t header;
    dns_question_t question;
    dns_answer_t answer;
} dns_packet_t;

// Function to parse DNS packet
int parse_dns_packet(uint8_t* buffer, size_t length, dns_packet_t* packet) {
    // Check if buffer is large enough
    if (length < sizeof(dns_header_t) + sizeof(dns_question_t) + sizeof(dns_answer_t)) {
        return -1;
    }

    // Parse DNS header
    packet->header.id = (uint16_t) ((buffer[0] << 8) | buffer[1]);
    packet->header.flags = (uint16_t) ((buffer[2] << 8) | buffer[3]);
    packet->header.qdcount = (uint16_t) ((buffer[4] << 8) | buffer[5]);
    packet->header.ancount = (uint16_t) ((buffer[6] << 8) | buffer[7]);
    packet->header.nscount = (uint16_t) ((buffer[8] << 8) | buffer[9]);
    packet->header.arcount = (uint16_t) ((buffer[10] << 8) | buffer[11]);

    // Parse DNS question
    packet->question.qtype = (uint16_t) ((buffer[12] << 8) | buffer[13]);
    packet->question.qclass = (uint16_t) ((buffer[14] << 8) | buffer[15]);

    // Parse DNS answer
    packet->answer.type = (uint16_t) ((buffer[16] << 8) | buffer[17]);
    packet->answer.class = (uint16_t) ((buffer[18] << 8) | buffer[19]);
    packet->answer.ttl = (uint32_t) ((buffer[20] << 24) | (buffer[21] << 16) | (buffer[22] << 8) | buffer[23]);
    packet->answer.rdlength = (uint16_t) ((buffer[24] << 8) | buffer[25]);

    return 0;
}

int main() {
    uint8_t buffer[] = {
        0x12, 0x34,  // ID
        0x01, 0x00,  // Flags
        0x00, 0x01,  // QDCOUNT
        0x00, 0x01,  // ANCOUNT
        0x00, 0x00,  // NSCOUNT
        0x00, 0x00,  // ARCOUNT
        0x00, 0x01,  // QTYPE
        0x00, 0x01,  // QCLASS
        0x00, 0x01,  // TYPE
        0x00, 0x01,  // CLASS
        0x00, 0x00, 0x00, 0x64,  // TTL
        0x00, 0x04   // RDLENGTH
    };

    dns_packet_t packet;
    int result = parse_dns_packet(buffer, sizeof(buffer), &packet);

    if (result == 0) {
        printf("DNS Packet:\n");
        printf("  ID: 0x%04x\n", packet.header.id);
        printf("  Flags: 0x%04x\n", packet.header.flags);
        printf("  QDCOUNT: %u\n", packet.header.qdcount);
        printf("  ANCOUNT: %u\n", packet.header.ancount);
        printf("  NSCOUNT: %u\n", packet.header.nscount);
        printf("  ARCOUNT: %u\n", packet.header.arcount);
        printf("  QTYPE: 0x%04x\n", packet.question.qtype);
        printf("  QCLASS: 0x%04x\n", packet.question.qclass);
        printf("  TYPE: 0x%04x\n", packet.answer.type);
        printf("  CLASS: 0x%04x\n", packet.answer.class);
        printf("  TTL: %u\n", packet.answer.ttl);
        printf("  RDLENGTH: %u\n", packet.answer.rdlength);
    } else {
        printf("Error parsing DNS packet\n");
    }

    return 0;
}