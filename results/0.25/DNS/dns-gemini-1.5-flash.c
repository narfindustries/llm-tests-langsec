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


unsigned char* create_dns_query(const char* hostname, unsigned short qtype, unsigned short qclass) {
    unsigned char *query;
    int hostname_len = strlen(hostname);
    int query_len = sizeof(dns_header) + hostname_len + sizeof(dns_question) + 1; //+1 for null terminator

    query = (unsigned char*)malloc(query_len);
    if (query == NULL) {
        perror("malloc failed");
        exit(1);
    }

    dns_header *header = (dns_header*)query;
    header->id = htons(getpid()); //Using process ID for ID
    header->flags = htons(0x0100); //Standard query
    header->qdcount = htons(1);
    header->ancount = htons(0);
    header->nscount = htons(0);
    header->arcount = htons(0);

    unsigned char *qname = query + sizeof(dns_header);
    int i = 0;
    char *token = strtok((char*)hostname, ".");
    while (token != NULL) {
        int len = strlen(token);
        qname[i++] = len;
        memcpy(qname + i, token, len);
        i += len;
        token = strtok(NULL, ".");
    }
    qname[i++] = 0;

    dns_question *question = (dns_question*)(qname + i);
    question->qtype = htons(qtype);
    question->qclass = htons(qclass);

    return query;
}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hostname>\n", argv[0]);
        return 1;
    }

    const char *hostname = argv[1];
    unsigned char *query = create_dns_query(hostname, 1, 1); //Type A, Class IN

    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        perror("socket creation failed");
        free(query);
        return 1;
    }

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(DNS_PORT);
    server_addr.sin_addr.s_addr = inet_addr("8.8.8.8"); //Google Public DNS

    int sent_bytes = sendto(sockfd, query, strlen((char*)query), 0, (struct sockaddr*)&server_addr, sizeof(server_addr));
    if (sent_bytes < 0) {
        perror("sendto failed");
        close(sockfd);
        free(query);
        return 1;
    }

    unsigned char buffer[MAX_PACKET_SIZE];
    int received_bytes = recvfrom(sockfd, buffer, MAX_PACKET_SIZE, 0, NULL, NULL);
    if (received_bytes < 0) {
        perror("recvfrom failed");
        close(sockfd);
        free(query);
        return 1;
    }

    printf("Received %d bytes\n", received_bytes);
    //Further processing of the received DNS response would go here.

    close(sockfd);
    free(query);
    return 0;
}
