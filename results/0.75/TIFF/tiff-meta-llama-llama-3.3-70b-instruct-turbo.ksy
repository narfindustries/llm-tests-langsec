meta:
  id: tiff
  file-extension: tif
  title: TIFF
  endianness: le
  license: mit
  page-size: 10

seq:
  - id: header
    type: tiff_header

types:
  tiff_header:
    seq:
      - id: magic
        type: u2
      - id: offset_to_ifd
        type: u4

  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: num_entries
      - id: next_ifd_offset
        type: u4

  ifd_entry:
    seq:
      - id: tag
        type: u2
      - id: type
        type: u2
      - id: num_values
        type: u4
      - id: value_offset
        type: u4
