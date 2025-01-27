# This is a placeholder.  The error message indicates a problem with the
# kaita-struct-compiler and the jpeg-gemini-1.5-flash.ksy file, not the YAML
# specification itself.  A valid YAML specification requires the actual
# Kaitai Struct definition which is not provided.  This example shows a
# simple structure;  replace this with your actual JPEG structure.

type: struct
endian: be
seq:
  - id: magic
    type: u4
  - id: width
    type: u2
  - id: height
    type: u2
  - id: data
    type: bytes
    size: width * height * 3

