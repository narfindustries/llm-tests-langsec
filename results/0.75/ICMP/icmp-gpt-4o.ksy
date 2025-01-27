meta:
  id: icmp_packet
  title: ICMP Packet
  application: network
  imports:
    - ipv4_packet
  endian: be

seq:
  - id: type
    type: u1
  - id: code
    type: u1
  - id: checksum
    type: u2
  - id: rest_of_header
    size: 4
  - id: data
    size-eos: true

types:
  ipv4_packet:
    seq:
      - id: version_ihl
        type: u1
      - id: dscp_ecn
        type: u1
      - id: total_length
        type: u2
      - id: identification
        type: u2
      - id: flags_fragment_offset
        type: u2
      - id: ttl
        type: u1
      - id: protocol
        type: u1
      - id: header_checksum
        type: u2
      - id: src_ip_addr
        type: b4
      - id: dst_ip_addr
        type: b4
      - id: options
        size: ((version_ihl & 0x0f) - 5) * 4
        if: (version_ihl & 0x0f) > 5
      - id: payload
        size-eos: true