meta:
  id: tiff
  file-extension: tiff
  endian: le

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: byte_order
        type: u2
        enum: endian
      - id: version
        type: u2
      - id: ifd_offset
        type: u4
    instances:
      first_ifd:
        pos: ifd_offset
        type: ifd

  ifd:
    seq:
      - id: num_dir_entries
        type: u2
      - id: entries
        type: directory_entry
        repeat: expr
        repeat-expr: num_dir_entries
      - id: next_ifd_offset
        type: u4
    instances:
      next_ifd:
        pos: next_ifd_offset
        type: ifd
        if: next_ifd_offset != 0

  directory_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_enum
      - id: field_type
        type: u2
        enum: field_type_enum
      - id: count
        type: u4
      - id: value_offset
        type: u4
    instances:
      value:
        pos: value_offset
        type:
          switch-on: field_type
          cases:
            'field_type_enum::byte': u1
            'field_type_enum::ascii': str_ascii
            'field_type_enum::short': u2
            'field_type_enum::long': u4
            'field_type_enum::rational': rational
            'field_type_enum::sbyte': s1
            'field_type_enum::undefined': u1
            'field_type_enum::sshort': s2
            'field_type_enum::slong': s4
            'field_type_enum::srational': srational
            'field_type_enum::float': f4
            'field_type_enum::double': f8
        size: count * type_size
        if: value_size > 4
      type_size:
        value: >-
          field_type == field_type_enum::byte ? 1 :
          field_type == field_type_enum::ascii ? 1 :
          field_type == field_type_enum::short ? 2 :
          field_type == field_type_enum::long ? 4 :
          field_type == field_type_enum::rational ? 8 :
          field_type == field_type_enum::sbyte ? 1 :
          field_type == field_type_enum::undefined ? 1 :
          field_type == field_type_enum::sshort ? 2 :
          field_type == field_type_enum::slong ? 4 :
          field_type == field_type_enum::srational ? 8 :
          field_type == field_type_enum::float ? 4 :
          field_type == field_type_enum::double ? 8 : 0
      value_size:
        value: count * type_size

  str_ascii:
    seq:
      - id: str
        type: str
        size-eos: true
        encoding: ASCII
        terminator: 0

  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

  srational:
    seq:
      - id: numerator
        type: s4
      - id: denominator
        type: s4

enums:
  endian:
    0x4949: le
    0x4d4d: be

  field_type_enum:
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

  tag_enum:
    254: subfile_type
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    263: threshholding
    264: cell_width
    265: cell_length
    266: fill_order
    269: document_name
    270: image_description
    271: make
    272: model
    273: strip_offsets
    277: samples_per_pixel
    278: rows_per_strip
    279: strip_byte_counts
    280: min_sample_value
    281: max_sample_value
    282: x_resolution
    283: y_resolution
    284: planar_configuration
    285: page_name
    286: x_position
    287: y_position
    288: free_offsets
    289: free_byte_counts
    290: gray_response_unit
    291: gray_response_curve
    296: resolution_unit
    320: color_map
    338: extra_samples
    33432: copyright