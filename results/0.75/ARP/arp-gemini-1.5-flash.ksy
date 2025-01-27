# This is a sample Kaitai Struct definition.  Adjust as needed for your specific data.
# The error message suggests a problem with the ksy file or the kaitai-struct-compiler itself.
# This example focuses on structure and data types, not specific ARP packet details.  You'll need to fill in those details.

type: struct
endian: be

fields:
  - id: header
    type: struct
    fields:
      - id: magic_number
        type: u4
      - id: version
        type: u2
      - id: packet_length
        type: u2
  - id: payload
    type: seq
    size: (header.packet_length - 8) # Adjust as needed.  8 is the size of the header.
    type: u1
