protocol ARP {
    // Ethernet header
    type EthernetHeader {
        destination_mac: [u8; 6],
        source_mac: [u8; 6], 
        ethertype: u16
    }

    // ARP packet structure
    type ARPPacket {
        hardware_type: u16,
        protocol_type: u16,
        hardware_size: u8,
        protocol_size: u8,
        opcode: u16,
        sender_mac: [u8; 6],
        sender_ip: [u8; 4],
        target_mac: [u8; 6],
        target_ip: [u8; 4]
    }

    // Full ARP frame with Ethernet header and ARP payload
    type ARPFrame {
        ethernet_header: EthernetHeader,
        arp_packet: ARPPacket
    }

    // Constants for ARP operations
    const ARP_REQUEST: u16 = 1;
    const ARP_REPLY: u16 = 2;

    // Ethernet type for ARP
    const ETHERTYPE_ARP: u16 = 0x0806;

    // Validate ARP packet structure and fields
    fn validate_arp_packet(packet: ARPPacket) -> bool {
        // Check hardware type (Ethernet)
        packet.hardware_type == 1 && 
        // Check protocol type (IPv4)
        packet.protocol_type == 0x0800 && 
        // Check hardware size (MAC address length)
        packet.hardware_size == 6 && 
        // Check protocol size (IPv4 address length)
        packet.protocol_size == 4 && 
        // Check valid opcode
        (packet.opcode == ARP_REQUEST || packet.opcode == ARP_REPLY)
    }

    // Parse ARP frame
    fn parse_arp_frame(frame: ARPFrame) -> Option<ARPPacket> {
        // Validate Ethernet header for ARP
        if frame.ethernet_header.ethertype == ETHERTYPE_ARP {
            // Validate ARP packet
            if validate_arp_packet(frame.arp_packet) {
                Some(frame.arp_packet)
            } else {
                None
            }
        } else {
            None
        }
    }
}