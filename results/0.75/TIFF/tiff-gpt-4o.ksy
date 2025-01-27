meta:
  id: tiff
  title: TIFF (Tagged Image File Format)
  file-extension: tiff
  xref:
    mime: image/tiff
  license: CC0-1.0
  endian: be

doc: |
  TIFF is a flexible, adaptable file format for handling images and data within a single file, by including the header tags (metadata) defining the image data, such as size, compression applied, color format, etc.

seq:
  - id: header
    type: header

types:
  header:
    seq:
      - id: byte_order
        type: u2
      - id: version
        type: u2
      - id: ifd0_offset
        type: u4

    instances:
      is_le:
        value: byte_order == 0x4949
      is_be:
        value: byte_order == 0x4d4d

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
      - id: count
        type: u4
      - id: value_offset
        type: u4