types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2le
      - id: version
        type: u2le
      - id: ifd_offset
        type: u4le
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
      - id: num_entries
        type: u2le
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd_offset
        type: u4le
  tiff_data_type:
    seq:
      - id: type
        type: u2le
      - id: count
        type: u4le
      - id: data
        type:
          switch-on: type
          cases:
            - 1:
                type: u1
            - 2:
                type: u1
            - 3:
                type: u2le
            - 4:
                type: u4le
            - 5:
                seq:
                  - id: numerator
                    type: u4le
                  - id: denominator
                    type: u4le
            - 6:
                type: s1
            - 7:
                type: u2le
            - 8:
                type: u4le
            - 9:
                type: s2le
            - 10:
                type: s4le
            - 11:
                seq:
                  - id: numerator
                    type: s4le
                  - id: denominator
                    type: s4le
            - 12:
                type: f4le
            - 13:
                type: f8le
            - 14:
                type: u1
  tiff_file:
    seq:
      - id: header
        type: tiff_header
      - id: ifd0
        type: tiff_ifd

