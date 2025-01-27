#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define MAX_PACKET_SIZE 512
#define DNS_PORT 53

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
    unsigned char *qname;
    unsigned short qtype;
    unsigned short qclass;
} dns_question;

// Function to convert a domain name to a byte array
unsigned char* domain_to_bytes(const char *domain) {
    char *token;
    char *rest = strdup(domain);
    unsigned char *bytes;
    int len = 0;
    int i;

    token = strtok_r(rest, ".", &rest);
    while (token != NULL) {
        len += strlen(token) + 1;
        token = strtok_r(rest, ".", &rest);
    }
    len++; //for null termination

    bytes = (unsigned char*)malloc(len);
    i = 0;
    token = strtok_r(strdup(domain), ".", &rest);
    while (token != NULL) {
        bytes[i++] = strlen(token);
        strcpy((char*)(bytes + i), token);
        i += strlen(token);
        token = strtok_r(rest, ".", &rest);
    }
    bytes[i] = 0;
    free(rest);
    return bytes;

}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <domain>\n", argv[0]);
        return 1;
    }

    char *domain = argv[1];
    unsigned char *qname_bytes = domain_to_bytes(domain);

    int sockfd;
    struct sockaddr_in server_addr;
    dns_header header;
    dns_question question;
    unsigned char buffer[MAX_PACKET_SIZE];
    int n;

    // Create socket
    sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        perror("Error creating socket");
        return 1;
    }

    // Set server address
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(DNS_PORT);
    server_addr.sin_addr.s_addr = inet_addr("8.8.8.8"); // Google DNS

    // Prepare DNS query
    header.id = htons(getpid());
    header.flags = htons(0x0100); // Standard query
    header.qdcount = htons(1);
    header.ancount = htons(0);
    header.nscount = htons(0);
    header.arcount = htons(0);

    question.qname = qname_bytes;
    question.qtype = htons(1); // A record
    question.qclass = htons(1); // IN

    // Assemble the query
    memcpy(buffer, &header, sizeof(header));
    memcpy(buffer + sizeof(header), &question, sizeof(question));
    memcpy(buffer + sizeof(header) + sizeof(question), qname_bytes, strlen((char*)qname_bytes)+1);


    // Send query
    n = sendto(sockfd, buffer, sizeof(header) + sizeof(question) + strlen((char*)qname_bytes) + 1, 0, (struct sockaddr *)&server_addr, sizeof(server_addr));
    if (n < 0) {
        perror("Error sending query");
        return 1;
    }

    // Receive response
    n = recvfrom(sockfd, buffer, MAX_PACKET_SIZE, 0, NULL, NULL);
    if (n < 0) {
        perror("Error receiving response");
        return 1;
    }


    // Process response (This is a placeholder -  needs to be implemented to extract IP addresses)
    printf("DNS Response received (%d bytes)\n", n);


    close(sockfd);
    free(qname_bytes);
    return 0;
}
