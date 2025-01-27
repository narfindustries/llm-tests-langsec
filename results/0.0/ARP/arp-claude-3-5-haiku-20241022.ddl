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

    // ARP operation codes
    const ARP_REQUEST: u16 = 1;
    const ARP_REPLY: u16 = 2;

    // Predefined Ethernet broadcast MAC address
    const BROADCAST_MAC: [u8; 6] = [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];

    // Ethernet type for ARP
    const ETHERTYPE_ARP: u16 = 0x0806;

    // Function to create an ARP request frame
    function create_arp_request(
        sender_mac: [u8; 6], 
        sender_ip: [u8; 4], 
        target_ip: [u8; 4]
    ) -> ARPFrame {
        ARPFrame {
            ethernet_header: EthernetHeader {
                destination_mac: BROADCAST_MAC,
                source_mac: sender_mac,
                ethertype: ETHERTYPE_ARP
            },
            arp_packet: ARPPacket {
                hardware_type: 1,  // Ethernet
                protocol_type: 0x0800,  // IPv4
                hardware_size: 6,  // MAC address length
                protocol_size: 4,  // IP address length
                opcode: ARP_REQUEST,
                sender_mac: sender_mac,
                sender_ip: sender_ip,
                target_mac: [0, 0, 0, 0, 0, 0],  // Unknown for request
                target_ip: target_ip
            }
        }
    }

    // Function to create an ARP reply frame
    function create_arp_reply(
        sender_mac: [u8; 6], 
        sender_ip: [u8; 4], 
        target_mac: [u8; 6], 
        target_ip: [u8; 4]
    ) -> ARPFrame {
        ARPFrame {
            ethernet_header: EthernetHeader {
                destination_mac: target_mac,
                source_mac: sender_mac,
                ethertype: ETHERTYPE_ARP
            },
            arp_packet: ARPPacket {
                hardware_type: 1,  // Ethernet
                protocol_type: 0x0800,  // IPv4
                hardware_size: 6,  // MAC address length
                protocol_size: 4,  // IP address length
                opcode: ARP_REPLY,
                sender_mac: sender_mac,
                sender_ip: sender_ip,
                target_mac: target_mac,
                target_ip: target_ip
            }
        }
    }
}