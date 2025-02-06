meta:
  id: icmp_packet
  title: Internet Control Message Protocol (ICMP)
  endian: be
  xref:
    rfc: 792

doc: |
  The Internet Control Message Protocol (ICMP) is used by network devices,
  like routers, to send error messages and operational information indicating
  issues such as communication failures and unreachable hosts. 

types:
  header:
    seq:
      - id: type
        type: u1
      - id: code
        type: u1
      - id: checksum
        type: u2

  echo:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: data
        size-eos: true

  timestamp:
    seq:
      - id: identifier
        type: u2
      - id: sequence_number
        type: u2
      - id: originate_timestamp
        type: u8
      - id: receive_timestamp
        type: u8
      - id: transmit_timestamp
        type: u8

  unreachable:
    seq:
      - id: unused
        type: u4
      - id: next_hop_mtu
        type: u2
        if: _parent.header.code == 4
      - id: internet_header_and_ip_payload
        size-eos: true

  time_exceeded:
    seq:
      - id: unused
        type: u4
      - id: internet_header_and_ip_payload
        size-eos: true
  
  parameter_problem:
    seq:
      - id: pointer
        type: u1
      - id: unused
        type: b24
      - id: internet_header_and_ip_payload
        size-eos: true

seq:
  - id: header
    type: header
  - id: body
    type:
      switch-on: header.type
      cases:
        0: echo  # Echo Reply
        3: unreachable  # Destination Unreachable
        4: echo  # Source Quench, structure same as Echo
        5: echo  # Redirect Message, assuming structure same as Echo
        8: echo  # Echo Request
        11: time_exceeded  # Time Exceeded
        12: parameter_problem  # Parameter Problem
        13: timestamp  # Timestamp
        14: timestamp  # Timestamp Reply
        15: echo  # Information Request (Deprecated)
        16: echo  # Information Reply (Deprecated)