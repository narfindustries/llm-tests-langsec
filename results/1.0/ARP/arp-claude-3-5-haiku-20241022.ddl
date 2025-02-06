module ARP {
    type arp_packet = {
        hardware_type: uint16;
        protocol_type: uint16;
        hardware_address_length: uint8;
        protocol_address_length: uint8;
        operation: uint16;
        sender_hardware_address: [uint8];
        sender_protocol_address: [uint8];
        target_hardware_address: [uint8];
        target_protocol_address: [uint8]
    }
}