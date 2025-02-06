meta:
  id: tiff
  file-extension: tif
  endian: le
  title: Tagged Image File Format (TIFF)
seq:
  - id: header
    type: header
  - id: ifds
    type: ifd
    repeat: eos
types:
  header:
    seq:
      - id: byte_order
        type: u2
        enum: endian
      - id: magic
        contents: [0x2A, 0x00]
      - id: offset_first_ifd
        type: u4
  ifd:
    seq:
      - id: num_dir_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: expr
        repeat-expr: num_dir_entries
      - id: next_ifd_offset
        type: u4
    instances:
      next_ifd:
        pos: next_ifd_offset
        type: ifd
        if: next_ifd_offset != 0
  ifd_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_type
      - id: type
        type: u2
        enum: field_type
      - id: count
        type: u4
      - id: value_offset
        type: u4
    instances:
      value:
        pos: value_offset
        size: _parent.entries[_index].type.size * count
        type:
          switch-on: type
          cases:
            'field_type::byte': u1
            'field_type::ascii': str
            'field_type::short': u2
            'field_type::long': u4
            'field_type::rational': rational
enums:
  endian:
    0x4949: le
    0x4D4D: be
  tag_type:
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    266: fill_order
    273: strip_offsets
    274: orientation
    277: samples_per_pixel
    278: rows_per_strip
    279: strip_byte_counts
    282: x_resolution
    283: y_resolution
    284: planar_configuration
    296: resolution_unit
  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational
types:
  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4