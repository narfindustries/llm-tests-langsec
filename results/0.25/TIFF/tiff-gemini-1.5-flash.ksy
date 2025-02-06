types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2le
      - id: version
        type: u2le
  ifd_entry:
    seq:
      - id: tag
        type: u2le
      - id: type
        type: u2le
      - id: count
        type: u4le
      - id: value_offset
        type: u4le
  tiff_ifd:
    seq:
      - id: entries
        type: seq
        repeat: expr
        repeat-expr: count_ifd_entries
        elem-type: ifd_entry
      - id: next_ifd_offset
        type: u4le
  tiff:
    seq:
      - id: header
        type: tiff_header
      - id: ifd0
        type: tiff_ifd
        
    instances:
      count_ifd_entries:
        expr: (ifd0.entries.len)

