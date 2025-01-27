meta:
  id: tiff
  file-extension: tif
  endian: le
seq:
  - id: file_header
    type: file_header
  - id: ifd0
    type: ifd
types:
  file_header:
    seq:
      - id: byte_order
        type: str
        size: 2
      - id: magic
        type: u2
      - id: ifd_offset
        type: u4
  ifd:
    seq:
      - id: num_tags
        type: u2
      - id: tags
        type: tag
        repeat: num_tags
      - id: next_ifd_offset
        type: u4
  tag:
    seq:
      - id: tag_number
        type: u2
      - id: tag_type
        type: u2
      - id: tag_count
        type: u4
      - id: tag_offset_or_value
        type:
          switch-on: tag_type
          cases:
            1: u1
            2: str
            3: u2
            4: u4
            5: u8
            7: u1
            9: u4
            10: u8