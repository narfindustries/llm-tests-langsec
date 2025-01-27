module ARP {
  import Network.IPv4
  import Network.Ethernet

  type ARP_Packet = struct {
    htype       : uint16  // Hardware type
    ptype       : uint16  // Protocol type
    hlen        : uint8   // Hardware address length
    plen        : uint8   // Protocol address length
    oper        : uint16  // Operation
    sha         : bytes @length(hlen)  // Sender hardware address
    spa         : IPv4.Address  // Sender protocol address
    tha         : bytes @length(hlen)  // Target hardware address
    tpa         : IPv4.Address  // Target protocol address
  }

  type Packet = struct {
    ethernetHeader : Ethernet.Header
    arpBody        : ARP_Packet
  }  
}