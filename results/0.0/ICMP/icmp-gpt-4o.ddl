module ICMP

// Define the ICMP header structure
struct ICMPHeader {
    type: uint8;          // Type of the ICMP message
    code: uint8;          // Code of the ICMP message
    checksum: uint16;     // Checksum for error-checking
    restOfHeader: uint32; // Rest of the header, varies by type and code
}

// Define the ICMP message structure
struct ICMPMessage {
    header: ICMPHeader;   // ICMP header
    payload: bytes;       // Payload of the ICMP message
}

// Define the main ICMP structure
struct ICMP {
    message: ICMPMessage; // ICMP message
}