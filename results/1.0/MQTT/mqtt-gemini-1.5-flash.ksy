# This is a sample Kaitai Struct definition.  You will need to replace this with your actual schema.
# The error message suggests a problem with the compilation command, not the schema itself.
# The error status 2 often indicates a problem with the input .ksy file, such as syntax errors.
# Please carefully review your original mqtt-gemini-1.5-flash.ksy file for syntax errors.

meta:
  id: mqtt-gemini-1
  endian: be

seq:
  - id: header
    type: u2
  - id: payload_length
    type: u4
  - id: payload
    type: str
    size: payload_length

