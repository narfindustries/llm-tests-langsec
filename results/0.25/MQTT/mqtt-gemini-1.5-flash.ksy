# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual schema.
# The error message suggests a problem with the compilation command, not necessarily the schema itself.
#  The error code 2 often indicates a problem with the input file (mqtt-gemini-1.5-flash.ksy).
#  Ensure this file exists and is correctly formatted according to the Kaitai Struct specification.

# Example:  A simple structure
type: struct
endian: be
fields:
  - id: magic_number
    type: u4
  - id: version
    type: u2
  - id: data_length
    type: u4
  - id: data
    type: bytes
    size: data_length

