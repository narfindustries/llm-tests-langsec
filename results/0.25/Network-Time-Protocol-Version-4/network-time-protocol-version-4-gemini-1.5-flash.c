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
    unsigned int transTm;
} ntp_packet;


int main() {
    // Create a socket
    int sockfd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sockfd == -1) {
        perror("socket creation failed");
        return 1;
    }

    // Configure the server address
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(123); // NTP port
    server_addr.sin_addr.s_addr = inet_addr("pool.ntp.org"); // Example NTP server

    // Create an NTP packet
    ntp_packet packet;
    memset(&packet, 0, sizeof(packet));
    packet.li_vn_mode = 0x1B; // Version 4, client mode

    // Send the NTP packet
    if (sendto(sockfd, &packet, sizeof(packet), 0, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1) {
        perror("sendto failed");
        close(sockfd);
        return 1;
    }

    // Receive the NTP packet
    socklen_t addr_len = sizeof(server_addr);
    if (recvfrom(sockfd, &packet, sizeof(packet), 0, (struct sockaddr *)&server_addr, &addr_len) == -1) {
        perror("recvfrom failed");
        close(sockfd);
        return 1;
    }

    // Extract the timestamp
    unsigned int timestamp = ntohl(packet.transTm);

    // Convert the timestamp to a time_t value
    time_t ntp_time = (time_t)(timestamp - 2208988800ULL); // NTP epoch to Unix epoch

    // Convert the time_t value to a human-readable format
    struct tm *tm = gmtime(&ntp_time);
    char time_str[100];
    strftime(time_str, sizeof(time_str), "%Y-%m-%d %H:%M:%S", tm);

    // Print the time
    printf("NTP time: %s\n", time_str);

    // Close the socket
    close(sockfd);

    return 0;
}
