meta:
  id: tiff
  title: TIFF
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
        enum: byte_order
      - id: version
        type: u2
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
        enum: tag
      - id: field_type
        type: u2
      - id: num_values
        type: u4
      - id: value_offset
        type: u4

enums:
  byte_order:
    0x4949: intel
    0x4d4d: motorola

  tag:
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    273: strip_offsets
    277: samples_per_pixel
    278: rows_per_strip
    279: strip_byte_counts
    282: x_resolution
    283: y_resolution
    284: planar_configuration
    296: resolution_unit
    320: color_map
    322: tile_width
    323: tile_length
    324: tile_offsets
    325: tile_byte_counts
    338: extra_samples
    339: sample_format

  compression:
    1: no_compression
    2: ccitt_huffman
    3: ccitt_t4
    4: ccitt_t6
    5: lzw
    6: jpeg
    32773: packbits

  photometric_interpretation:
    0: white_is_zero
    1: black_is_zero
    2: rgb
    3: palette_color
    4: transparency_mask
    5: cmyk
    6: ycbcr
    8: cielab

  resolution_unit:
    1: no_absolute_unit
    2: inch
    3: centimeter

  planar_configuration:
    1: chunky
    2: planar

  extra_samples:
    0: unspecified
    1: associated_alpha
    2: unassociated_alpha

  sample_format:
    1: unsigned_integer
    2: signed_integer
    3: ieee_floating_point
    4: undefined