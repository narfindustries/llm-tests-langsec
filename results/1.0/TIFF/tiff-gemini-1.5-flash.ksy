types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2be
      - id: version
        type: u2be
      - id: ifd_offset
        type: u4be
  ifd_entry:
    seq:
      - id: tag
        type: u2be
      - id: type
        type: u2be
      - id: count
        type: u4be
      - id: value_or_offset
        type: u4be
  tiff_ifd:
    seq:
      - id: num_entries
        type: u2be
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd_offset
        type: u4be
  rational:
    seq:
      - id: numerator
        type: u4be
      - id: denominator
        type: u4be
  tiff_file:
    seq:
      - id: header
        type: tiff_header
      - id: ifd0
        type: tiff_ifd
      - id: ifds
        type: tiff_ifd
        repeat: expr
        repeat-expr: ifd0.next_ifd_offset != 0
        if: ifd0.next_ifd_offset != 0

