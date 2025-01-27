protocol ARP {
    // Ethernet frame type for ARP
    const ARP_ETHERTYPE = 0x0806;

    // ARP operation codes
    const ARP_REQUEST = 1;
    const ARP_REPLY = 2;

    // Hardware address types
    const HARDWARE_TYPE_ETHERNET = 1;

    // Protocol address types
    const PROTOCOL_TYPE_IPV4 = 0x0800;

    // ARP packet structure
    packet ArpPacket {
        hardware_type: uint16,
        protocol_type: uint16,
        hardware_addr_length: uint8,
        protocol_addr_length: uint8,
        operation: uint16,
        sender_mac: macaddr,
        sender_ip: ipv4addr,
        target_mac: macaddr,
        target_ip: ipv4addr
    }

    // ARP packet parsing and generation
    function parse(data: bytes) -> ArpPacket {
        let packet = ArpPacket {
            hardware_type: data[0:2] as uint16,
            protocol_type: data[2:4] as uint16,
            hardware_addr_length: data[4] as uint8,
            protocol_addr_length: data[5] as uint8,
            operation: data[6:8] as uint16,
            sender_mac: data[8:14] as macaddr,
            sender_ip: data[14:18] as ipv4addr,
            target_mac: data[18:24] as macaddr,
            target_ip: data[24:28] as ipv4addr
        };
        return packet;
    }

    function generate(packet: ArpPacket) -> bytes {
        return 
            packet.hardware_type.to_bytes(2) +
            packet.protocol_type.to_bytes(2) +
            packet.hardware_addr_length.to_bytes(1) +
            packet.protocol_addr_length.to_bytes(1) +
            packet.operation.to_bytes(2) +
            packet.sender_mac.to_bytes() +
            packet.sender_ip.to_bytes() +
            packet.target_mac.to_bytes() +
            packet.target_ip.to_bytes();
    }

    // ARP request generation
    function create_request(
        sender_mac: macaddr, 
        sender_ip: ipv4addr, 
        target_ip: ipv4addr
    ) -> ArpPacket {
        return ArpPacket {
            hardware_type: HARDWARE_TYPE_ETHERNET,
            protocol_type: PROTOCOL_TYPE_IPV4,
            hardware_addr_length: 6,
            protocol_addr_length: 4,
            operation: ARP_REQUEST,
            sender_mac: sender_mac,
            sender_ip: sender_ip,
            target_mac: macaddr(0, 0, 0, 0, 0, 0),
            target_ip: target_ip
        };
    }

    // ARP reply generation
    function create_reply(
        sender_mac: macaddr, 
        sender_ip: ipv4addr, 
        target_mac: macaddr, 
        target_ip: ipv4addr
    ) -> ArpPacket {
        return ArpPacket {
            hardware_type: HARDWARE_TYPE_ETHERNET,
            protocol_type: PROTOCOL_TYPE_IPV4,
            hardware_addr_length: 6,
            protocol_addr_length: 4,
            operation: ARP_REPLY,
            sender_mac: sender_mac,
            sender_ip: sender_ip,
            target_mac: target_mac,
            target_ip: target_ip
        };
    }
}