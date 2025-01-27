module ARP {
  import Network.IPv4

  type MAC = bytes<6>;
  type IPv4 = Network.IPv4.IPv4Addr;

  type HardwareType = uint16 {
    Ethernet = 1
  };

  type ProtocolType = uint16 {
    IP = 0x0800
  };

  type Operation = uint16 {
    Request = 1,
    Reply = 2
  };

  type ARPPacket = struct {
    htype: HardwareType,
    ptype: ProtocolType,
    hlen: uint8,
    plen: uint8,
    oper: Operation,
    sha: MAC,
    spa: IPv4,
    tha: MAC,
    tpa: IPv4
  };

  entrypoint parse: ARPPacket;
}