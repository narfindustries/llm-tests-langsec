types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2
      - id: version
        type: u2
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
  tiff_ifd:
    seq:
      - id: entry_count
        type: u2
      - id: entries
        type: seq
          type: ifd_entry
          repeat: expr
            expr: this.entry_count
      - id: next_ifd_offset
        type: u4
  tiff_file:
    seq:
      - id: header
        type: tiff_header
      - id: ifd0
        type: tiff_ifd
      - id: ifd1
        type: tiff_ifd
        if: this.ifd0.next_ifd_offset != 0
      - id: ifd_chain
        type: repeat_until
          until: lambda this: this.next_ifd_offset == 0
          element:
            seq:
              - id: ifd
                type: tiff_ifd
              - id: next_ifd_offset
                type: u4
                if: this.ifd.next_ifd_offset != 0

