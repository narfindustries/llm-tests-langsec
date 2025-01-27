meta:
  id: tiff
  file-extension: tif
  endian: le
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
    instances:
      ifd:
        type: ifd
        pos: offset_to_ifd
  ifd:
    seq:
      - id: num_entries
        type: u2
      - id: entries
        type: ifd_entry
        repeat: num_entries
      - id: next_ifd_offset
        type: u4
    instances:
      next_ifd:
        type: ifd
        pos: next_ifd_offset
        if: next_ifd_offset != 0
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
    instances:
      value:
        type:
          switch-on: type
          cases:
            1: # BYTE
              type: u1
              repeat: num_values
            2: # ASCII
              type: s1
              repeat: num_values
            3: # SHORT
              type: u2
              repeat: num_values
            4: # LONG
              type: u4
              repeat: num_values
            5: # RATIONAL
              type: u8
              repeat: num_values
        pos: value_offset