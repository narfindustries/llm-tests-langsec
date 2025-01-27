# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual schema.
# The error message suggests a problem with the compilation command, not necessarily the schema itself.
#  The error code 2 often indicates a problem with the input file (nitf-gemini-1.5-flash.ksy).
#  Double-check the file's contents for syntax errors or inconsistencies.

# Example:  A simple structure
type: struct
endian: be

fields:
  - id: header
    type: u4
  - id: data
    type: str
    size: header
