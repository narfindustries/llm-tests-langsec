#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define MAX_PACKET_SIZE 512
#define MAX_DOMAIN_NAME 255

// Structure to represent a DNS header
typedef struct {
    unsigned short id;
    unsigned short flags;
    unsigned short qdcount;
    unsigned short ancount;
    unsigned short nscount;
    unsigned short arcount;
} dns_header;

// Structure to represent a DNS question
typedef struct {
    unsigned char qname[MAX_DOMAIN_NAME];
    unsigned short qtype;
    unsigned short qclass;
} dns_question;

// Function to convert domain name to bytes
int domain_to_bytes(const char *domain, unsigned char *bytes) {
    int i = 0;
    char *token;
    char *rest = (char *)domain;

    while ((token = strtok_r(rest, ".", &rest))) {
        int len = strlen(token);
        bytes[i++] = len;
        memcpy(bytes + i, token, len);
        i += len;
    }
    bytes[i++] = 0;
    return i;
}


int main() {
    //Example usage (replace with your actual DNS query)
    char *domain = "www.example.com";
    unsigned char buffer[MAX_PACKET_SIZE];
    dns_header *header = (dns_header *)buffer;
    dns_question *question = (dns_question *)(buffer + sizeof(dns_header));

    header->id = htons(12345); // Random ID
    header->flags = htons(0x0100); // Standard query
    header->qdcount = htons(1);
    header->ancount = htons(0);
    header->nscount = htons(0);
    header->arcount = htons(0);

    domain_to_bytes(domain, question->qname);
    question->qtype = htons(1); // A record
    question->qclass = htons(1); // IN class

    //Send the DNS query (replace with your actual sending mechanism)
    //This is a placeholder and needs to be implemented based on your networking requirements.
    printf("DNS Query for %s generated.\n", domain);
    printf("Query size: %zu bytes\n", sizeof(dns_header) + strlen(domain) + 1 + sizeof(dns_question));
    // ... (Code to send the buffer over a socket) ...

    return 0;
}
