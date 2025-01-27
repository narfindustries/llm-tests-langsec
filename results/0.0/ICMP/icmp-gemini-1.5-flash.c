#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>

#define PACKET_SIZE 64

// ICMP header structure
struct icmp_header {
    unsigned char type;
    unsigned char code;
    unsigned short checksum;
    unsigned short id;
    unsigned short sequence;
};


unsigned short checksum(unsigned short *buf, int len) {
    unsigned long sum = 0;
    for (int i = 0; i < len; i++) {
        sum += buf[i];
    }
    sum = (sum >> 16) + (sum & 0xFFFF);
    sum += (sum >> 16);
    return ~sum;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <target_ip> <count>\n", argv[0]);
        return 1;
    }

    char *target_ip = argv[1];
    int count = atoi(argv[2]);

    int sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (sockfd < 0) {
        perror("socket");
        return 1;
    }

    struct sockaddr_in dest_addr;
    memset(&dest_addr, 0, sizeof(dest_addr));
    dest_addr.sin_family = AF_INET;
    dest_addr.sin_addr.s_addr = inet_addr(target_ip);


    char packet[PACKET_SIZE];
    struct icmp_header *icmp_header = (struct icmp_header *)packet;
    icmp_header->type = ICMP_ECHO;
    icmp_header->code = 0;
    icmp_header->id = getpid();
    icmp_header->sequence = 0;

    for (int i = 0; i < count; i++) {
        icmp_header->sequence = i + 1;
        icmp_header->checksum = 0;
        icmp_header->checksum = checksum((unsigned short *)icmp_header, PACKET_SIZE);

        if (sendto(sockfd, packet, PACKET_SIZE, 0, (struct sockaddr *)&dest_addr, sizeof(dest_addr)) < 0) {
            perror("sendto");
            close(sockfd);
            return 1;
        }
    }

    close(sockfd);
    return 0;
}
