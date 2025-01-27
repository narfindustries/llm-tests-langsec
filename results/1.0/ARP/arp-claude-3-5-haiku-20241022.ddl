format arp {
    // Define structure for ARP packets
    record ARPPacket {
        hardware_type: uint16,         // Hardware type (Ethernet = 1)
        protocol_type: uint16,         // Protocol type (IPv4 = 0x0800)
        hardware_addr_length: uint8,   // Hardware address length (6 for MAC)
        protocol_addr_length: uint8,   // Protocol address length (4 for IPv4)
        operation: uint16,              // ARP operation (1 = request, 2 = reply)
        
        sender_mac: uint48,            // Sender MAC address 
        sender_ip: uint32,             // Sender IP address
        
        target_mac: uint48,            // Target MAC address
        target_ip: uint32              // Target IP address
    }
    
    // Define ARP packet parsing rules
    parse ARPPacket {
        // Validate ARP packet structure
        constraint hardware_type == 1;
        constraint protocol_type == 0x0800;
        constraint hardware_addr_length == 6;
        constraint protocol_addr_length == 4;
        
        // Valid operation codes
        constraint operation in [1, 2];
    }
}