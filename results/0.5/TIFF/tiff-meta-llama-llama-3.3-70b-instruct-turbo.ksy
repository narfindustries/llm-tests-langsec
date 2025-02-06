meta:
  id: tiff
  endian: le

seq:
  - id: ifd_offset
    type: u4
  - id: ifd
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: entry
        repeat: num_entries
      - id: next_ifd_offset
        type: u4

types:
  u1: int 1
  u2: int 2
  u4: int 4
  str: str null-terminated
  rational:
    seq:
      - id: numerator
        type: u4
      - id: denominator
        type: u4

entry:
  seq:
    - id: tag
      type: u2
    - id: type
      type: u2
    - id: num_values
      type: u4
    - id: value_offset
      type: u4
  enums:
    type:
      1: byte
      2: ascii
      3: short
      4: long
      5: rational
      7: undefined
      9: signed long
      10: signed rational
    tiff_tags:
      254: subfile_type
      255: image_width
      256: image_length
      257: bits_per_sample
      258: compression
      262: photometric_interpretation
      274: orientation
      277: samples_per_pixel
      278: rows_per_strip
      279: strip_offsets
      280: strip_byte_counts
      281: min_sample_value
      282: max_sample_value
      283: x_resolution
      284: y_resolution
      285: planar_configuration
      296: resolution_unit
      305: software
      306: date_time
      315: artist
      316: host_computer
      317: predictor
      318: white_point
      319: primary_chromaticities
      520: ycbcr_coefficients
      521: ycbcr_sub_sampling
      522: ycbcr_positioning
      529: reference_black_white
      530: related_image_file_format
      531: related_image_width
      532: related_image_length
  instances:
    ImageWidth:
      tag: 256
      type: 3
      num_values: 1
      value: u4
    ImageLength:
      tag: 257
      type: 3
      num_values: 1
      value: u4
    BitsPerSample:
      tag: 258
      type: 3
      num_values: 3
      value: u2
    Compression:
      tag: 259
      type: 3
      num_values: 1
      value: u2
    PhotometricInterpretation:
      tag: 262
      type: 3
      num_values: 1
      value: u2
    Orientation:
      tag: 274
      type: 3
      num_values: 1
      value: u2
    SamplesPerPixel:
      tag: 277
      type: 3
      num_values: 1
      value: u2
    PlanarConfiguration:
      tag: 284
      type: 3
      num_values: 1
      value: u2
    TileWidth:
      tag: 322
      type: 4
      num_values: 1
      value: u4
    TileLength:
      tag: 323
      type: 4
      num_values: 1
      value: u4
    TileOffsets:
      tag: 324
      type: 4
      repeat: expr
      value: u4
    TileByteCounts:
      tag: 325
      type: 4
      repeat: expr
      value: u4
    SubFileType:
      tag: 254
      type: 4
      num_values: 1
      value: u4
    DateTime:
      tag: 306
      type: 2
      num_values: 20
      value: str
    ImageDescription:
      tag: 270
      type: 2
      num_values: expr
      value: str
    Make:
      tag: 271
      type: 2
      num_values: expr
      value: str
    Model:
      tag: 272
      type: 2
      num_values: expr
      value: str
    Software:
      tag: 305
      type: 2
      num_values: expr
      value: str
    Artist:
      tag: 315
      type: 2
      num_values: expr
      value: str
    Copyright:
      tag: 33432
      type: 2
      num_values: expr
      value: str
    YCbCrSubSampling:
      tag: 530
      type: 3
      num_values: 2
      value: u2
    YCbCrPositioning:
      tag: 531
      type: 3
      num_values: 1
      value: u2
    ReferenceBlackWhite:
      tag: 532
      type: 3
      num_values: 6
      value: u2
    XResolution:
      tag: 282
      type: 5
      num_values: 1
      value: rational
    YResolution:
      tag: 283
      type: 5
      num_values: 1
      value: rational
    ResolutionUnit:
      tag: 296
      type: 3
      num_values: 1
      value: u2