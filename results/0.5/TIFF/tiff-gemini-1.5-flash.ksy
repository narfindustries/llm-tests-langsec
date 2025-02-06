types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2
      - id: version
        type: u2
  tiff_ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: field_type
        type: u2
      - id: count
        type: u4
      - id: value_offset
        type: u4
      - id: value:
          type: any
  tiff_ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: array
          type: tiff_ifd_entry
          read: num_entries
  tiff_file:
    seq:
      - id: header
        type: tiff_header
      - id: ifd0
        type: tiff_ifd
      - id: next_ifd_offset
        type: u4
        if: ifd0.num_entries > 0
      - id: data
        type: bytes
        size: (next_ifd_offset - header.size)
        if: ifd0.num_entries > 0

