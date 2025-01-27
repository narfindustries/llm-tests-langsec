meta:
  id: tiff
  file-extension: tif
  endian: le
  title: TIFF (Tagged Image File Format)
  license: CC0-1.0
seq:
  - id: header
    type: header
  - id: ifds
    type: ifd
    repeat: eos

types:
  header:
    seq:
      - id: endian
        type: u2
        valid:
          any-of: [0x4949, 0x4d4d]
      - id: magic
        type: u2
        valid:
          eq: 42
      - id: offset
        type: u4

  ifd:
    seq:
      - id: num_fields
        type: u2
      - id: fields
        type: field
        repeat: expr
        repeat-expr: num_fields
      - id: next_ifd
        type: u4
    instances:
      next_ifd_inst:
        pos: next_ifd
        type: ifd
        if: next_ifd != 0

  field:
    seq:
      - id: tag
        type: u2
      - id: type
        enum: field_type
        type: u2
      - id: length
        type: u4
      - id: value_offset
        type: u4
    instances:
      value:
        io: _root._io
        pos: value_offset
        size: length * type_size
        type:
          switch-on: type
          cases:
            'field_type::byte': data_bytes
            'field_type::ascii': data_ascii
            'field_type::short': data_short
            'field_type::long': data_long
            'field_type::rational': data_rational

enums:
  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational

types:
  data_bytes:
    seq:
      - id: bytes
        size: _parent.length
        type: u1
  data_ascii:
    seq:
      - id: string
        type: str
        encoding: ASCII
        size: _parent.length
  data_short:
    seq:
      - id: shorts
        type: u2
        repeat: expr
        repeat-expr: _parent.length
  data_long:
    seq:
      - id: longs
        type: u4
        repeat: expr
        repeat-expr: _parent.length
  data_rational:
    seq:
      - id: value
        type: rational
        repeat: expr
        repeat-expr: _parent.length
  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4
