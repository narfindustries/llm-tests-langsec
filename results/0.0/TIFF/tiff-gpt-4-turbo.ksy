meta:
  id: tiff
  file-extension: tif
  endian: le
  title: Tagged Image File Format (TIFF)
doc: |
  TIFF is a flexible, adaptable file format for handling images and data within a single file, by including the header tags (size, definition, image-data arrangement, applied image compression) defining the image's geometry. For example, a TIFF file can be a container holding compressed (lossy) JPEG and (lossless) PackBits compressed images. TIFF files also can include vector-based clipping paths (outlines, croppings, image frames). Other TIFF options include multiple subfiles, each of which can be of a different nature and might be individually compressed and/or encrypted.
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
        valid:
          any-of: [0x4949, 0x4d4d]
      - id: magic
        type: u2
        valid:
          eq: 42
      - id: offset
        type: u4
        doc: Offset to the first IFD

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
      - id: offset
        type: u4
    instances:
      data:
        pos: offset
        size: count * field_type_size
        if: count * field_type_size > 4

enums:
  endian:
    0x4949: le
    0x4d4d: be

  tag_type:
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
    296: resolution_unit
    # Additional tags can be added here

  field_type:
    1: byte
    2: ascii
    3: short
    4: long
    5: rational

instances:
  field_type_size:
    value: |
      switch (type) {
        case 1: return 1; // byte
        case 2: return 1; // ascii
        case 3: return 2; // short
        case 4: return 4; // long
        case 5: return 8; // rational (2 longs)
        default: return 0;
      }
    type: u4