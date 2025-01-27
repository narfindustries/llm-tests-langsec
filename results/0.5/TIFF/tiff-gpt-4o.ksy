meta:
  id: tiff
  title: TIFF
  file-extension: tiff
  endian: be
  encoding: ASCII

seq:
  - id: header
    type: tiff_header

types:
  tiff_header:
    seq:
      - id: signature
        contents: [0x49, 0x49, 0x2A, 0x00] # Little-endian
        if: _root.endian == 'le'
      - id: signature
        contents: [0x4D, 0x4D, 0x00, 0x2A] # Big-endian
        if: _root.endian == 'be'
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
      - id: count
        type: u4
      - id: value_offset
        type: u4