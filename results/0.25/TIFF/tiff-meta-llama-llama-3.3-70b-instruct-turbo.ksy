meta:
  id: tiff
  endian: le

seq:
  - id: byte_order
    type: str
    size: 2
    encoding: ascii
    enum: II MM
  - id: ifd_offset
    type: u4

types:
  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: entry
        repeat: expr
        repeat-expr: num_entries
      - id: next_ifd_offset
        type: u4

  entry:
    seq:
      - id: tag
        type: u2
      - id: type
        type: u2
        enum: 1 2 3 4 5 7 9 10 11
      - id: num_values
        type: u4
      - id: value_offset
        type: u4

  tags:
    type: object
    seq:
      - id: image_width
        type: u2 or u4
        key: 0x0100
      - id: image_length
        type: u2 or u4
        key: 0x0101
      - id: bits_per_sample
        type: u2
        key: 0x0102
      - id: compression
        type: u2
        enum: 1 2 3 4 5 6 7 8 9 10 32766 32767 32768 32769 32809 32895 32908 32909 32946 32947
        key: 0x0103
      - id: photometric_interpretation
        type: u2
        enum: 0 1 2 3 4 5 6
        key: 0x0106
      - id: orientation
        type: u2
        enum: 1 2 3 4 5 6 7 8
        key: 0x010e
      - id: samples_per_pixel
        type: u2
        key: 0x0115
      - id: planar_configuration
        type: u2
        enum: 1 2
        key: 0x011c
      - id: ycbcr_sub_sampling
        type: u2
        enum: 1 2 4
        key: 0x0212
      - id: ycbcr_positioning
        type: u2
        enum: 1 2
        key: 0x0213
      - id: x_resolution
        type: u5
        key: 0x011a
      - id: y_resolution
        type: u5
        key: 0x011b
      - id: resolution_unit
        type: u2
        enum: 1 2 3
        key: 0x0128
      - id: date_time
        type: str
        encoding: ascii
        key: 0x0132
      - id: artist
        type: str
        encoding: ascii
        key: 0x013b
      - id: image_description
        type: str
        encoding: ascii
        key: 0x010e_1
      - id: make
        type: str
        encoding: ascii
        key: 0x010e_2
      - id: model
        type: str
        encoding: ascii
        key: 0x0110
      - id: software
        type: str
        encoding: ascii
        key: 0x0131
      - id: date_time_original
        type: str
        encoding: ascii
        key: 0x9003
      - id: date_time_digitized
        type: str
        encoding: ascii
        key: 0x9004
      - id: sub_ifds
        type: u4
        key: 0x014a
      - id: transfer_function
        type: u2
        key: 0x012d
      - id: white_point
        type: u5
        key: 0x013e
      - id: primary_chromaticities
        type: u5
        key: 0x013f
      - id: color_map
        type: u2
        key: 0x0140
      - id: halftone_hints
        type: u2
        key: 0x0141
      - id: tile_width
        type: u4
        key: 0x0142
      - id: tile_length
        type: u4
        key: 0x0143
      - id: tile_offsets
        type: u4
        key: 0x0144
      - id: tile_byte_counts
        type: u4
        key: 0x0145
      - id: sub_ifd_offset
        type: u4
        key: 0x014a_1
      - id: ink_set
        type: u2
        enum: 1 2
        key: 0x014c
      - id: ink_names
        type: str
        encoding: ascii
        key: 0x014d
      - id: number_of_inks
        type: u2
        key: 0x014e
      - id: dot_range
        type: u2
        key: 0x0150
      - id: target_printer
        type: str
        encoding: ascii
        key: 0x0151
      - id: extra_samples
        type: u2
        enum: 0 1 2
        key: 0x0152
      - id: sample_format
        type: u2
        enum: 1 2 3 4 5 6
        key: 0x0153
      - id: s_min_sample_value
        type: u2 or u4 or u5 or i2 or i4 or i5
        key: 0x0154
      - id: s_max_sample_value
        type: u2 or u4 or u5 or i2 or i4 or i5
        key: 0x0155
      - id: transfer_range
        type: u2 or u4 or u5 or i2 or i4 or i5
        key: 0x0156
      - id: clip_path
        type: str
        encoding: ascii
        key: 0x0157
      - id: x_clip_path_units
        type: u5
        key: 0x0158
      - id: y_clip_path_units
        type: u5
        key: 0x0159
      - id: indexed
        type: u2
        enum: 0 1
        key: 0x015a
      - id: jpeg_tables
        type: bytes
        key: 0x015b
      - id: opi_proxy
        type: bytes
        key: 0x015c
      - id: global_parameters_ifd
        type: u4
        key: 0x015d
      - id: profile_type
        type: u4
        key: 0x015e
      - id: fax_tone_range
        type: u2
        enum: 0 1
        key: 0x015f
      - id: coding_methods
        type: u2
        enum: 0 1 2
        key: 0x0160
      - id: version_year
        type: u4
        key: 0x0161
      - id: mode_number
        type: u2
        key: 0x0162
      - id: decode
        type: u2 or u5
        key: 0x0163
      - id: default_image_color
        type: u2
        key: 0x0164
      - id: t82_options
        type: u2
        key: 0x0165
      - id: jpeg_proc
        type: u2
        enum: 1 14
        key: 0x0200
      - id: jpeg_interchange_format
        type: u4
        key: 0x0201
      - id: jpeg_interchange_format_length
        type: u4
        key: 0x0202
      - id: jpeg_restart_interval
        type: u2
        key: 0x0203
      - id: jpeg_lossless_predictors
        type: u2
        key: 0x0205
      - id: jpeg_point_transforms
        type: u2
        key: 0x0206
      - id: jpeg_q_tables
        type: bytes
        key: 0x0207
      - id: jpeg_dctables
        type: bytes
        key: 0x0208
      - id: jpeg_actables
        type: bytes
        key: 0x0209
      - id: ycbcr_coefficients
        type: u5
        key: 0x0211
      - id: ycbcr_sub_sampling_1
        type: u2
        enum: 1 2 4
        key: 0x0212_1
      - id: ycbcr_positioning_1
        type: u2
        enum: 1 2
        key: 0x0213_1
      - id: reference_black_white
        type: u5
        key: 0x0214
      - id: xml_packet
        type: bytes
        key: 0x02bc
      - id: image_id
        type: str
        encoding: ascii
        key: 0x8000
      - id: wang_tag1
        type: u4
        key: 0x80a3
      - id: wang_tag2
        type: u4
        key: 0x80a4
      - id: wang_tag3
        type: u4
        key: 0x80a5
      - id: wang_tag4
        type: u4
        key: 0x80a6
      - id: image_source_data
        type: bytes
        key: 0x8218
      - id: intergraph_packet_data
        type: bytes
        key: 0x847e
      - id: intergraph_matrix
        type: u2
        key: 0x8480