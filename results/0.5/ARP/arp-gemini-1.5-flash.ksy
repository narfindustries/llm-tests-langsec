meta:
  id: arp-gemini-1
  endian: be

types:
  header:
    seq: u2
    length: u2
    type: u2
    subtype: u2

  data:
    - id: type_data
      type: seq
      contents:
        - id: data_type
          type: u2
        - id: data_length
          type: u2
        - id: data_value
          type: array
          type: u1
          read: data_length


seq:
  - id: header
    type: header
  - id: data_block
    type: data

