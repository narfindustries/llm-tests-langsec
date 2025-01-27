meta:
  id: tiff
  file-extension: tif
  endian: le
seq:
  - id: header
    type: tiff_header
types:
  tiff_header:
    seq:
      - id: byte_order
        type: u1
      - id: version
        type: u2
      - id: ifd_offset
        type: u4
  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: num_entries
      - id: next_ifd_offset
        type: u4
  ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: type
        type: u2
      - id: num_values
        type: u4
      - id: value_offset
        type: u4