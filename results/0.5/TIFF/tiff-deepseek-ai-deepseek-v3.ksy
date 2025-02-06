meta:
  id: tiff
  title: Tagged Image File Format
  file-extension: tiff
  license: MIT
  endian: le
seq:
  - id: header
    type: tiff_header
  - id: ifds
    type: ifd
    repeat: eos
types:
  tiff_header:
    seq:
      - id: byte_order
        type: u2
        enum: byte_order
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
  ifd_entry:
    seq:
      - id: tag
        type: u2
        enum: tag_type
      - id: field_type
        type: u2
        enum: field_type
      - id: count
        type: u4
      - id: value_offset
        type: u4
    instances:
      value:
        pos: value_offset
        size: |
          field_type == field_type::byte ? count :
          field_type == field_type::ascii ? count :
          field_type == field_type::short ? count * 2 :
          field_type == field_type::long ? count * 4 :
          field_type == field_type::rational ? count * 8 :
          field_type == field_type::sbyte ? count :
          field_type == field_type::undefined ? count :
          field_type == field_type::sshort ? count * 2 :
          field_type == field_type::slong ? count * 4 :
          field_type == field_type::srational ? count * 8 :
          field_type == field_type::float ? count * 4 :
          field_type == field_type::double ? count * 8 :
          0
        type: |
          field_type == field_type::byte ? u1 :
          field_type == field_type::ascii ? str :
          field_type == field_type::short ? u2 :
          field_type == field_type::long ? u4 :
          field_type == field_type::rational ? u8 :
          field_type == field_type::sbyte ? s1 :
          field_type == field_type::undefined ? u1 :
          field_type == field_type::sshort ? s2 :
          field_type == field_type::slong ? s4 :
          field_type == field_type::srational ? s8 :
          field_type == field_type::float ? f4 :
          field_type == field_type::double ? f8 :
          void
enums:
  byte_order:
    0x4949: little_endian
    0x4d4d: big_endian
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
  tag_type:
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
    270: image_description
    271: make
    272: model
    273: strip_offsets
    274: orientation
    277: samples_per_pixel
    278: rows_per_strip
    279: strip_byte_counts
    280: min_sample_value
    281: max_sample_value
    282: x_resolution
    283: y_resolution
    284: planar_configuration
    288: free_offsets
    289: free_byte_counts
    290: gray_response_unit
    291: gray_response_curve
    296: resolution_unit
    297: page_number
    305: software
    306: date_time
    315: artist
    316: host_computer
    317: predictor
    318: white_point
    319: primary_chromaticities
    320: color_map
    321: halftone_hints
    322: tile_width
    323: tile_length
    324: tile_offsets
    325: tile_byte_counts
    338: extra_samples
    339: sample_format
    340: s_min_sample_value
    341: s_max_sample_value
    346: transfer_range
    529: ycbcr_coefficients
    530: ycbcr_sub_sampling
    531: ycbcr_positioning
    532: reference_black_white
    33432: copyright
    32995: matteing
    32996: data_type
    32997: image_depth
    32998: tile_depth