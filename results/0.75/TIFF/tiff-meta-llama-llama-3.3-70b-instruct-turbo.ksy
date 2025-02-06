meta:
  id: tiff
  endian: expr
  params:
    - id: byte_order
      type: str
      required: true
      enum: ["le", "be"]

  doc: TIFF (Tagged Image File Format) is a file format for storing raster images.
  doc-ref: https://en.wikipedia.org/wiki/TIFF

seq:
  - id: header
    type: header
  - id: ifd
    type: ifd

types:
  header:
    seq:
      - id: byte_order
        type: str
        size: 2
        enum: ["II", "MM"]
      - id: magic
        type: uint16
        value: 42
      - id: ifd_offset
        type: uint32

  ifd_entry:
    seq:
      - id: tag
        type: uint16
      - id: type
        type: uint16
      - id: count
        type: uint32
      - id: value_offset
        type: uint32
      - id: value
        type:
          switch-on: type
          cases:
            1: uint8
            2: str
            3: uint16
            4: uint32
            5: rational
            7: bytes
            9: int32
            10: srational
            11: float
            12: double

  ifd:
    seq:
      - id: num_entries
        type: uint16
      - id: entries
        type: ifd_entry
        repeat: expr
      - id: next_ifd_offset
        type: uint32

  rational:
    seq:
      - id: numerator
        type: uint32
      - id: denominator
        type: uint32

  srational:
    seq:
      - id: numerator
        type: int32
      - id: denominator
        type: int32

enum:
  compression:
    values:
      - 1: No compression
      - 2: CCITT Group 3
      - 3: CCITT Group 4
      - 4: LZW
      - 5: PackBits
      - 6: JPEG (old-style)
      - 7: JPEG (new-style)
      - 8: Deflate
      - 9: Adobe Deflate
  photometric_interpretation:
    values:
      - 0: WhiteIsZero
      - 1: BlackIsZero
      - 2: RGB
      - 3: Palette color
      - 4: Transparency mask
      - 5: CMYK
      - 6: YCbCr
      - 7: CIE L*a*b*
  orientation:
    values:
      - 1: Top-left
      - 2: Top-right
      - 3: Bottom-right
      - 4: Bottom-left
      - 5: Left-top
      - 6: Right-top
      - 7: Right-bottom
      - 8: Left-bottom
  planar_configuration:
    values:
      - 1: Chunky (contiguous)
      - 2: Planar (separate planes)
  data_types:
    values:
      - 1: BYTE
      - 2: ASCII
      - 3: SHORT
      - 4: LONG
      - 5: RATIONAL
      - 7: UNDEFINED
      - 9: SLONG
      - 10: SRATIONAL
      - 11: FLOAT
      - 12: DOUBLE

expr:
  - id: num_entries
    value: 10

  - id: endian
    value: header.byte_order == "II" ? "le" : "be"