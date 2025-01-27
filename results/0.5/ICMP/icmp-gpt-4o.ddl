module ICMP {
    // Define the ICMP header structure
    struct ICMPHeader {
        type: uint8;         // ICMP message type
        code: uint8;         // ICMP message code
        checksum: uint16;    // ICMP checksum
        restOfHeader: uint32; // Rest of the header, varies by type and code
    }

    // Define a union for different ICMP message types
    union ICMPMessage {
        case (header.type == 0) EchoReply;
        case (header.type == 8) EchoRequest;
        // Add more cases for other ICMP message types if needed
    }

    // Structure for ICMP Echo Request/Reply messages
    struct EchoRequest {
        identifier: uint16;  // Identifier
        sequenceNumber: uint16; // Sequence number
        payload: bytes;      // Payload data
    }

    struct EchoReply {
        identifier: uint16;  // Identifier
        sequenceNumber: uint16; // Sequence number
        payload: bytes;      // Payload data
    }

    // Main ICMP packet structure
    struct ICMPPacket {
        header: ICMPHeader;
        message: ICMPMessage;
    }
}