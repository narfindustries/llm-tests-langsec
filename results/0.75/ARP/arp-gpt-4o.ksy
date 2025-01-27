meta:
  id: arp_packet
  file-extension: arp
  endian: be
  title: ARP Packet

doc: |
  Address Resolution Protocol (ARP) is a communication protocol used for discovering
  the link layer address, like a MAC address, associated with a given internet layer
  address, typically an IPv4 address.

seq:
  - id: hardware_type
    type: u2
    doc: |
      Hardware type specifies the network protocol type. For Ethernet, this is usually 1.

  - id: protocol_type
    type: u2
    doc: |
      Protocol type specifies the internetwork protocol for which the ARP request is intended.
      For IPv4, this is 0x0800.

  - id: hardware_size
    type: u1
    doc: |
      Length in octets of a hardware address. Ethernet addresses size is 6.

  - id: protocol_size
    type: u1
    doc: |
      Length in octets of internetwork addresses. IPv4 address size is 4.

  - id: opcode
    type: u2
    doc: |
      Operation code which specifies the type of ARP message. Typically 1 for request and 2 for reply.

  - id: sender_mac_address
    type: mac_address
    doc: |
      Hardware address of the sender of the ARP message.

  - id: sender_ip_address
    type: ipv4_address
    doc: |
      IPv4 address of the sender of the ARP message.

  - id: target_mac_address
    type: mac_address
    doc: |
      Hardware address of the intended receiver of the ARP message.

  - id: target_ip_address
    type: ipv4_address
    doc: |
      IPv4 address of the intended receiver of the ARP message.

types:
  mac_address:
    seq:
      - id: octets
        type: u1
        repeat: expr
        repeat-expr: 6
        doc: Physical hardware address (MAC address).

  ipv4_address:
    seq:
      - id: octets
        type: u1
        repeat: expr
        repeat-expr: 4
        doc: IPv4 address.
