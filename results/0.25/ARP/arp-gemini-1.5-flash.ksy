# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual data.
# The error message suggests a problem with the compilation command, not the .ksy file itself.
#  The error likely stems from issues outside the scope of this .ksy file (e.g., missing dependencies, incorrect paths).

#  This example demonstrates a simple structure.  Adapt it to your needs.

type: struct
endian: be

fields:
  - id: magic
    type: u4
  - id: version
    type: u2
  - id: data_length
    type: u2
  - id: data
    type: seq
    size: data_length
    type: u1

