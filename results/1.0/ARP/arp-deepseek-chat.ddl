// ARP (Address Resolution Protocol) Packet Specification
// Version 1.0
// Author: DeepSeek Chat

struct ARPPacket {
  hardware_type: UInt16; // Hardware type (e.g., Ethernet = 1)
  protocol_type: UInt16; // Protocol type (e.g., IPv4 = 0x0800)
  hardware_size: UInt8;  // Hardware address length (e.g., Ethernet = 6)
  protocol_size: UInt8;  // Protocol address length (e.g., IPv4 = 4)
  opcode: UInt16;        // Operation code (1 = request, 2 = reply)
  sender_hardware_addr: Bytes[hardware_size]; // Sender hardware address
  sender_protocol_addr: Bytes[protocol_size]; // Sender protocol address
  target_hardware_addr: Bytes[hardware_size]; // Target hardware address
  target_protocol_addr: Bytes[protocol_size]; // Target protocol address
}

// Example usage:
// let arp_packet = ARPPacket {
//   hardware_type: 1,
//   protocol_type: 0x0800,
//   hardware_size: 6,
//   protocol_size: 4,
//   opcode: 1,
//   sender_hardware_addr: [0x00, 0x0C, 0x29, 0x15, 0x5A, 0x3E],
//   sender_protocol_addr: [192, 168, 1, 100],
//   target_hardware_addr: [0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
//   target_protocol_addr: [192, 168, 1, 1]
// };