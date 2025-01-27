meta:
  id: tiff-gemini-1
  title: TIFF Gemini 1.5 Flash
  endian: be

types:
  ifd_entry:
    seq:
      - tag: u2
      - field_type: u2
      - count: u4
      - value_offset: u4

  ifd:
    seq:
      - entry_count: u2
      - entries:
          type: ifd_entry
          repeat: expr(entry_count)

  tiff_header:
    seq:
      - byte_order: u2
      - version: u2
      - ifd_offset: u4


seq:
  - header: tiff_header
  - ifd:
      if header.byte_order == 0x4949: # II
        endian: le
      else: # MM
        endian: be

