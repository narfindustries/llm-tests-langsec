#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>

// Define the NTP packet structure
typedef struct {
    unsigned int li_vn_mode:8;
    unsigned int stratum:8;
    unsigned int poll:8;
    unsigned int precision:8;
    unsigned int rootDelay:32;
    unsigned int rootDispersion:32;
    unsigned int refId:32;
    unsigned int refTm:32;
    unsigned int origTm:32;
    unsigned int recvTm:32;
    unsigned int xmtTm:32;
} ntp_packet;


int main() {
    int sockfd;
    struct sockaddr_in servaddr;
    ntp_packet packet;
    time_t now;

    // Create a UDP socket
    sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd < 0) {
        perror("socket creation failed");
        exit(EXIT_FAILURE);
    }

    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(123); // NTP port
    //Using a reliable NTP server
    if (inet_pton(AF_INET, "pool.ntp.org", &servaddr.sin_addr)<=0) {
        perror("inet_pton error occured");
        exit(EXIT_FAILURE);
    }

    // Set up the NTP packet
    packet.li_vn_mode = 0x1B; // Version 4, client mode
    packet.stratum = 0;
    packet.poll = 0;
    packet.precision = 0;
    packet.rootDelay = 0;
    packet.rootDispersion = 0;
    packet.refId = 0;
    packet.refTm = 0;
    packet.origTm = 0;
    packet.recvTm = 0;
    now = time(NULL);
    packet.xmtTm = htonl(now);


    // Send the NTP packet
    sendto(sockfd, (const char*)&packet, sizeof(packet), 0, (const struct sockaddr*)&servaddr, sizeof(servaddr));

    // Receive the NTP packet (optional - for round trip time calculation)
    recvfrom(sockfd, (char*)&packet, sizeof(packet), 0, NULL, NULL);

    // Close the socket
    close(sockfd);

    printf("NTP request sent successfully.\n");
    return 0;
}
