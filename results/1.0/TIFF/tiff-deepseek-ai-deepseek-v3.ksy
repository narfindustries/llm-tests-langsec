meta:
  id: tiff
  file-extension: tiff
  endian: le
seq:
  - id: header
    type: tiff_header
  - id: ifds
    type: ifd
    repeat: until
    repeat-until: _.next_ifd_offset == 0
types:
  tiff_header:
    seq:
      - id: byte_order
        type: str
        size: 2
        encoding: ASCII
      - id: version
        type: u2
      - id: first_ifd_offset
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
    instances:
      next_ifd:
        pos: next_ifd_offset
        type: ifd
  ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: field_type
        type: u2
      - id: count
        type: u4
      - id: value_or_offset
        type: u4
    instances:
      value:
        pos: value_or_offset
        type:
          switch-on: field_type
          cases:
            1: u1
            2: strz
            3: u2
            4: u4
            5: rational
            6: s1
            7: bytes
            8: s2
            9: s4
            10: f4
            11: f8
    enums:
      tag:
        254: new_subfile_type
        255: subfile_type
        256: image_width
        257: image_length
        258: bits_per_sample
        259: compression
        262: photometric_interpretation
        263: thresholding
        264: cell_width
        265: cell_length
        266: fill_order
        269: document_name
        270: image_description
        271: make
        272: model
        273: strip_offsets
        274: orientation
        277: samples_per_pixel
        278: rows_per_strip
        279: strip_byte_counts
        282: x_resolution
        283: y_resolution
        284: planar_configuration
        296: resolution_unit
        305: software
        306: date_time
        320: color_map
        338: extra_samples
        339: sample_format
        347: jpeg_tables
        529: ycbcr_coefficients
        530: ycbcr_sub_sampling
        531: ycbcr_positioning
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
        10: float
        11: double
  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4
  strz:
    seq:
      - id: value
        type: str
        encoding: ASCII
        terminator: 0