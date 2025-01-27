meta:
  id: tiff-gemini-1
  title: TIFF Gemini 1.5 Flash
  homepage: ""
  authors: []
  license: ""
  version: 0.25

types:
  byte: u1
  word: u2
  dword: u4
  qword: u8

seq:
  - id: header
    type: header_struct
  - id: ifd_entries
    type: ifd_entry
    repeat: expr
    repeat-expr: header.ifd_offset != 0
  - id: ifd_entries_next
    type: ifd_entry
    repeat: expr
    repeat-expr: header.next_ifd_offset != 0


structs:
  header_struct:
    seq:
      - id: byte_order
        type: byte
      - id: version
        type: word
      - id: ifd_offset
        type: dword
      - id: next_ifd_offset
        type: dword

  ifd_entry:
    seq:
      - id: tag
        type: word
      - id: type
        type: word
      - id: count
        type: dword
      - id: value_offset
        type: dword

