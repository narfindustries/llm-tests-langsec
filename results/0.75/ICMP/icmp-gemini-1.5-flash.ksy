# This is a sample Kaitai Struct definition.  Adjust as needed for your actual data.
# Error message suggests a problem with the compilation process, not the schema itself.
# The schema below is a placeholder and may need significant modification to accurately represent your ICMP data.

types:
  icmp_header:
    seq:
      - id: type
        type: u1
      - id: code
        type: u1
      - id: checksum
        type: u2le
      - id: identifier
        type: u2be
      - id: sequence_number
        type: u2be
      - id: data
        type: bytes
        size: lambda this: this.body_len


  icmp_packet:
    seq:
      - id: header
        type: icmp_header
      - id: body
        type: bytes
        size: lambda this: this.header.body_len

