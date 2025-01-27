meta:
  id: tiff
  title: Tagged Image File Format
  file-extension: tiff
  endian: be

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: magic
        contents: [0x49, 0x49, 0x2A, 0x00]
      - id: ifd0_ofs
        type: u4le
        
  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd_ofs
        type: u4

  ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: field_type
        type: u2
      - id: num_values
        type: u4
      - id: value_or_ofs
        type: u4

    instances:
      value:
        io: _root._io
        pos: value_or_ofs
        repeat: expr
        repeat-expr: num_values
        size-eos: false
        type:
          switch-on: field_type
          cases:
            '1': u1   # BYTE
            '2': str  # ASCII
            '3': u2   # SHORT
            '4': u4   # LONG
            '5': rational  # RATIONAL

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4