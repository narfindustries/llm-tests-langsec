type HardwareType = uint16;
type ProtocolType = uint16;
type HardwareLength = uint8;
type ProtocolLength = uint8;
type Operation = uint16;

type MACAddress = bytes[6];
type IPAddress = bytes[4];

type ARP_Packet = struct {
  htype : HardwareType,
  ptype : ProtocolType,
  hlen : HardwareLength,
  plen : ProtocolLength,
  oper : Operation,
  sha : MACAddress,
  spa : IPAddress,
  tha : MACAddress,
  tpa : IPAddress
};