meta:
  id: tiff
  title: TIFF (Tagged Image File Format) Image File
  file-extension: 
    - tif
    - tiff
  endian: 
    little

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
        enum: endian_type
      - id: version
        type: u2
      - id: first_ifd_offset
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

  field:
    seq:
      - id: tag
        type: u2
        enum: tag_type
      - id: field_type
        type: u2
        enum: type_type
      - id: num_values
        type: u4
      - id: value_or_offset
        type: u4

enums:
  endian_type:
    0x4949: little
    0x4D4D: big

  tag_type:
    254: new_subfile_type
    256: image_width
    257: image_length
    258: bits_per_sample
    259: compression
    262: photometric_interpretation
    273: strip_offsets
    277: samples_per_pixel
    278: rows_per_strip
    279: strip_byte_counts
    284: planar_configuration
    
  type_type:
    1: ubyte
    2: ascii
    3: ushort
    4: ulong
    5: urational