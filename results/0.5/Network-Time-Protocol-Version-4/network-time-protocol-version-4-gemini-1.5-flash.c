#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>

// Structure for NTP packet
typedef struct {
    unsigned char li_vn_mode;
    unsigned char stratum;
    unsigned char poll;
    unsigned char precision;
    unsigned long long rootDelay;
    unsigned long long rootDispersion;
    unsigned long long refId;
    unsigned long long refTm;
    unsigned long long origTm;
    unsigned long long recvTm;
    unsigned long long xmtTm;
} ntp_packet;


int main() {
    int sockfd;
    struct sockaddr_in servaddr;
    ntp_packet packet;
    time_t now;

    // Create socket
    if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
        perror("socket creation failed");
        exit(EXIT_FAILURE);
    }

    memset(&servaddr, 0, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(123); // NTP port
    //Change this to a real NTP server address
    if(inet_pton(AF_INET, "pool.ntp.org", &servaddr.sin_addr)<=0) {
        perror("inet_pton error occured");
        exit(EXIT_FAILURE);
    }


    //Set up NTP packet
    packet.li_vn_mode = 0x1B; // li=0, vn=4, mode=3 (client)
    packet.stratum = 0;
    packet.poll = 0;
    packet.precision = 0;
    packet.rootDelay = 0;
    packet.rootDispersion = 0;
    packet.refId = 0;
    packet.refTm = 0;
    packet.origTm = 0;
    packet.recvTm = 0;
    packet.xmtTm = 0;

    time(&now);
    packet.xmtTm = htonl((unsigned long long)now);


    // Send NTP request
    if (sendto(sockfd, (const char*)&packet, sizeof(packet), 0, (const struct sockaddr*)&servaddr, sizeof(servaddr)) < 0) {
        perror("sendto failed");
        exit(EXIT_FAILURE);
    }

    // Receive NTP response (simplified - error handling omitted for brevity)
    if (recvfrom(sockfd, (char*)&packet, sizeof(packet), 0, NULL, NULL) < 0) {
        perror("recvfrom failed");
        exit(EXIT_FAILURE);
    }
    
    //Extract timestamp (simplified)
    unsigned long long ntp_time = ntohl(packet.xmtTm);

    printf("NTP Time: %llu\n", ntp_time);


    close(sockfd);
    return 0;
}
