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

// Structure for DNS header
typedef struct {
    unsigned short id;
    unsigned short flags;
    unsigned short qdcount;
    unsigned short ancount;
    unsigned short nscount;
    unsigned short arcount;
} dns_header;

// Structure for DNS question
typedef struct {
    unsigned short qtype;
    unsigned short qclass;
} dns_question;


unsigned char* build_dns_query(const char* hostname, unsigned short qtype, unsigned short qclass) {
    unsigned char *query;
    int hostname_len = strlen(hostname);
    int query_len = sizeof(dns_header) + 2 + hostname_len + 4;
    query = (unsigned char*)malloc(query_len);
    if (query == NULL) {
        perror("malloc");
        exit(1);
    }

    dns_header *header = (dns_header*)query;
    header->id = htons(getpid());
    header->flags = htons(0x0100); // Standard query
    header->qdcount = htons(1);
    header->ancount = htons(0);
    header->nscount = htons(0);
    header->arcount = htons(0);

    query += sizeof(dns_header);
    int i = 0;
    for (; i < hostname_len; i++) {
        int label_len = 0;
        while (i < hostname_len && hostname[i] != '.') {
            i++;
            label_len++;
        }
        query[0] = label_len;
        query++;
        memcpy(query, hostname + i - label_len, label_len);
        query += label_len;
        if (i < hostname_len) {
            i++;
        }
    }
    query[0] = 0;
    query++;

    dns_question *question = (dns_question*)query;
    question->qtype = htons(qtype);
    question->qclass = htons(qclass);

    return query - (query_len - sizeof(dns_header)) ;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hostname>\n", argv[0]);
        return 1;
    }

    const char *hostname = argv[1];
    unsigned char *query = build_dns_query(hostname, 1, 1); // Type A, Class IN

    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd == -1) {
        perror("socket");
        free(query);
        return 1;
    }

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(DNS_PORT);
    server_addr.sin_addr.s_addr = inet_addr("8.8.8.8"); // Google Public DNS

    if (sendto(sockfd, query, strlen((char*)query) + sizeof(dns_header), 0, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        perror("sendto");
        close(sockfd);
        free(query);
        return 1;
    }

    unsigned char buffer[MAX_PACKET_SIZE];
    socklen_t server_len = sizeof(server_addr);
    int bytes_received = recvfrom(sockfd, buffer, MAX_PACKET_SIZE, 0, (struct sockaddr*)&server_addr, &server_len);
    if (bytes_received == -1) {
        perror("recvfrom");
        close(sockfd);
        free(query);
        return 1;
    }

    close(sockfd);
    free(query);
    //Further processing of the DNS response can be added here.

    return 0;
}
