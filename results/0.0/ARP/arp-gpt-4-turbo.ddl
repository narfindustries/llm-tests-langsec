module ARP {
  import Network::IPv4;

  type MACAddress = bytes[6];
  type IPv4Address = Network::IPv4::Address;

  type Header = struct {
    htype       : uint16;  // Hardware type
    ptype       : uint16;  // Protocol type
    hlen        : uint8;   // Hardware address length
    plen        : uint8;   // Protocol address length
    oper        : uint16;  // Operation
    sha         : MACAddress;  // Sender hardware address
    spa         : IPv4Address; // Sender protocol address
    tha         : MACAddress;  // Target hardware address
    tpa         : IPv4Address; // Target protocol address
  }
}