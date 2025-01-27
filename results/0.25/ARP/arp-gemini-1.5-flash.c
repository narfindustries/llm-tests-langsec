#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent an ARP entry
typedef struct {
    unsigned char ip_address[4];
    unsigned char mac_address[6];
} arp_entry_t;

// Function to add an ARP entry
int add_arp_entry(arp_entry_t *arp_table, int table_size, unsigned char *ip_address, unsigned char *mac_address) {
    for (int i = 0; i < table_size; i++) {
        if (memcmp(arp_table[i].ip_address, ip_address, 4) == 0) {
            // IP address already exists, update MAC address
            memcpy(arp_table[i].mac_address, mac_address, 6);
            return 0; // Success
        }
    }

    // IP address not found, add new entry if space available
    for (int i = 0; i < table_size; i++) {
        if (memcmp(arp_table[i].ip_address, "\0\0\0\0", 4) == 0) {
            memcpy(arp_table[i].ip_address, ip_address, 4);
            memcpy(arp_table[i].mac_address, mac_address, 6);
            return 0; // Success
        }
    }

    return -1; // Table full
}


// Function to resolve an IP address to a MAC address
int resolve_ip(arp_entry_t *arp_table, int table_size, unsigned char *ip_address, unsigned char *mac_address) {
    for (int i = 0; i < table_size; i++) {
        if (memcmp(arp_table[i].ip_address, ip_address, 4) == 0) {
            memcpy(mac_address, arp_table[i].mac_address, 6);
            return 0; // Success
        }
    }
    return -1; // IP address not found
}

int main() {
    // Example usage
    arp_entry_t arp_table[100]; // ARP table with a maximum of 100 entries
    memset(arp_table, 0, sizeof(arp_table)); // Initialize ARP table

    unsigned char ip1[] = {192, 168, 1, 100};
    unsigned char mac1[] = {0x00, 0x16, 0x3e, 0x00, 0x00, 0x01};

    unsigned char ip2[] = {192, 168, 1, 101};
    unsigned char mac2[] = {0x00, 0x16, 0x3e, 0x00, 0x00, 0x02};


    add_arp_entry(arp_table, 100, ip1, mac1);
    add_arp_entry(arp_table, 100, ip2, mac2);

    unsigned char resolved_mac[6];
    if (resolve_ip(arp_table, 100, ip1, resolved_mac) == 0) {
        printf("MAC address for %d.%d.%d.%d: ", ip1[0], ip1[1], ip1[2], ip1[3]);
        for (int i = 0; i < 6; i++) {
            printf("%02x", resolved_mac[i]);
            if (i < 5) printf(":");
        }
        printf("\n");
    } else {
        printf("IP address not found in ARP table.\n");
    }

    return 0;
}
