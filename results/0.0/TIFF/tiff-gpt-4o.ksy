meta:
  id: tiff
  title: TIFF
  file-extension: tiff
  endian: be

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: magic_number
        contents: [0x49, 0x49, 0x2A, 0x00]
      - id: ifd_offset
        type: u4

  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd_offset
        type: u4

  ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: field_type
        type: u2
      - id: num_values
        type: u4
      - id: value_offset
        type: u4