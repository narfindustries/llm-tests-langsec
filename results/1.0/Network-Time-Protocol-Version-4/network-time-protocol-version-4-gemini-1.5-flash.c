#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <unistd.h>

// Define NTP packet structure
typedef struct {
    unsigned int li_vn_mode;
    unsigned int stratum;
    unsigned int poll;
    unsigned int precision;
    unsigned int rootDelay;
    unsigned int rootDispersion;
    unsigned int refId;
    unsigned int refTm;
    unsigned int origTm;
    unsigned int recvTm;
    unsigned int xmtTm;
} ntp_packet;


int main() {
    int sockfd;
    struct sockaddr_in servaddr;
    ntp_packet packet;
    unsigned char buffer[48];

    // Create socket
    sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        perror("socket creation failed");
        exit(EXIT_FAILURE);
    }

    // Set server address
    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_addr.s_addr = inet_addr("pool.ntp.org"); // Example NTP server
    servaddr.sin_port = htons(123);

    // Initialize NTP packet
    memset(&packet, 0, sizeof(packet));
    packet.li_vn_mode = 0x1b; // Version 4, Client mode


    // Convert packet to network byte order
    packet.li_vn_mode = htonl(packet.li_vn_mode);
    //Other fields can be filled if needed, but this is a minimal example.

    // Copy packet to buffer
    memcpy(buffer, &packet, sizeof(packet));

    // Send packet
    sendto(sockfd, buffer, sizeof(packet), 0, (const struct sockaddr*)&servaddr, sizeof(servaddr));

    // Receive packet (optional)
    recvfrom(sockfd, buffer, sizeof(packet), 0, NULL, NULL);

    //Process received packet (optional)
    memcpy(&packet, buffer, sizeof(packet));
    packet.li_vn_mode = ntohl(packet.li_vn_mode);
    //Further processing of received data as needed


    // Close socket
    close(sockfd);

    return 0;
}
