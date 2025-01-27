# Please replace placeholders like 999999 with actual values.  This is a 
# sample and may require adjustments based on your specific ARP data.

types:
  arp_header:
    seq:
      - id: hardware_type
        type: u2
      - id: protocol_type
        type: u2
      - id: hardware_size
        type: u1
      - id: protocol_size
        type: u1
      - id: opcode
        type: u2
      - id: sender_mac
        type: array
        type: u1
        size: 6
      - id: sender_ip
        type: ip4
      - id: target_mac
        type: array
        type: u1
        size: 6
      - id: target_ip
        type: ip4

  arp_packet:
    seq:
      - id: header
        type: arp_header
      - id: padding #Add padding if needed to match actual packet size
        type: array
        type: u1
        size: lambda: self.header.padding_size # Calculate dynamically

#Example calculation of padding, adjust as needed
# padding_size = lambda: max(0, self.size - self.header.size)

