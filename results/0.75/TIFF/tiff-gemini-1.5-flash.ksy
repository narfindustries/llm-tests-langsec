# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual schema.
# The error message suggests a problem with the kaitai-struct-compiler command and the .ksy file.
#  This example focuses on a simple structure to demonstrate correct YAML formatting.  
#  You will need to adapt it to your specific TIFF structure.


meta:
  id: simple_structure
  endian: be

seq:
  - id: header
    type: u4
  - id: data
    type: u4
    repeat: expr
    repeat-expr: header


