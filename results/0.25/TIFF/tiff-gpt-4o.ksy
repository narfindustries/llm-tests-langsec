meta:
  id: tiff
  title: TIFF (Tagged Image File Format)
  file-extension: tiff
  xref:
    mime: image/tiff
  license: CC0-1.0
  endian: le

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: byte_order
        contents: ['II', 'MM']
      - id: version
        contents: [42]
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

enums:
  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational
    6: sbyte
    7: undefined
    8: sshort
    9: slong
    10: srational
    11: float
    12: double

instances:
  ifds:
    pos: header.ifd_offset
    type: ifd
    repeat: until
    repeat-until: _.next_ifd_offset == 0
    size-eos: true