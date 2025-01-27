#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <arpa/inet.h>

// Structure to represent a NTP packet
typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t rootDelay;
    uint32_t rootDispersion;
    uint32_t refId;
    uint64_t refTm;
    uint64_t origTm;
    uint64_t recvTm;
    uint64_t tranTm;
} ntp_packet;


int main() {
    //Example usage (replace with your actual NTP server address and port)
    char* server_address = "pool.ntp.org";
    int server_port = 123;

    //  (Error handling and network communication omitted for brevity.  
    //   This is a simplified example and requires substantial network code to function.)

    ntp_packet packet;

    // ... Network code to receive NTP packet from server ...


    //Example of accessing the received time (requires proper byte order conversion)
    time_t ntp_time = (time_t)(ntohl(packet.recvTm));


    printf("Received NTP time: %s", ctime(&ntp_time));

    return 0;
}
