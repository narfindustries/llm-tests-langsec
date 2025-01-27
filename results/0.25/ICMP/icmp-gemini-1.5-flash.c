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
#define DEST_IP "127.0.0.1" //Change to target IP

unsigned short checksum(unsigned short *buf, int len) {
    unsigned long sum = 0;
    for (int i = 0; i < len; i++)
        sum += buf[i];
    sum = (sum >> 16) + (sum & 0xFFFF);
    sum += (sum >> 16);
    return ~sum;
}

int main() {
    int sockfd;
    struct sockaddr_in dest;
    char packet[PACKET_SIZE];
    struct icmphdr *icmp = (struct icmphdr *)packet;
    struct timeval tv;

    sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (sockfd < 0) {
        perror("socket");
        exit(1);
    }

    memset(&dest, 0, sizeof(dest));
    dest.sin_family = AF_INET;
    dest.sin_addr.s_addr = inet_addr(DEST_IP);

    icmp->type = ICMP_ECHO;
    icmp->code = 0;
    icmp->checksum = 0;
    icmp->un.echo.id = getpid();
    icmp->un.echo.sequence = 0;

    //Data payload
    memset(packet + sizeof(struct icmphdr), 'A', PACKET_SIZE - sizeof(struct icmphdr));

    icmp->checksum = checksum((unsigned short *)packet, PACKET_SIZE);

    while (1) {
        if (sendto(sockfd, packet, PACKET_SIZE, 0, (struct sockaddr *)&dest, sizeof(dest)) < 0) {
            perror("sendto");
            exit(1);
        }
        //Added a small delay to avoid overwhelming the target
        usleep(10000); // 10ms delay
    }

    close(sockfd);
    return 0;
}
