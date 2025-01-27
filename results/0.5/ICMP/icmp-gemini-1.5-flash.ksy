types:
  icmp_header:
    seq:
      type: u2
    checksum:
      type: u2
    type:
      type: u1
    code:
      type: u1
  icmp_gemini_packet:
    header:
      type: icmp_header
    data:
      type: seq
      size: 20


