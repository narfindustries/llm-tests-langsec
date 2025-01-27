module ARP {
  type MAC = bytes[6]
  type IPv4 = bytes[4]

  def checksum(data: bytes) -> u16 = 
    foldl(data, 0u16) { |acc, x| acc + u16::from_be_bytes([x, 0x00]) }
  
  type Header = struct {
    htype: u16,      // Hardware type
    ptype: u16,      // Protocol type
    hlen: u8,        // Hardware address length
    plen: u8,        // Protocol address length
    oper: u16,       // Operation
    sha: MAC,        // Sender hardware address
    spa: IPv4,       // Sender protocol address
    tha: MAC,        // Target hardware address
    tpa: IPv4        // Target protocol address
  }
  
  type Packet = struct {
    header: Header,
    chksum: u16
  }

  let arpPacket = Packet(
    header: Header(
      htype = 0x0001, // Ethernet
      ptype = 0x0800, // IPv4
      hlen  = 0x06,   // MAC length
      plen  = 0x04,   // IPv4 length
      oper  = 0x0001, // Request
      sha   = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06],
      spa   = [0xC0, 0xA8, 0x01, 0x01],
      tha   = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
      tpa   = [0xC0, 0xA8, 0x01, 0x02]
    ),
    chksum = checksum(bytes.concat(
      u16::to_be_bytes(0x0001), // htype
      u16::to_be_bytes(0x0800), // ptype
      [0x06],                   // hlen
      [0x04],                   // plen
      u16::to_be_bytes(0x0001), // oper
      [0x01, 0x02, 0x03, 0x04, 0x05, 0x06], // sha
      [0xC0, 0xA8, 0x01, 0x01], // spa
      [0x00, 0x00, 0x00, 0x00, 0x00, 0x00], // tha
      [0xC0, 0xA8, 0x01, 0x02]  // tpa
    ))
  )
}