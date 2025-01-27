# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual schema.
# The error message suggests a problem with the compilation command, not necessarily the schema itself.
# The path 'generated/999999/0.75/Modbus/modbus-gemini-1.5-flash.ksy' indicates a file structure issue.
# Ensure this path exists and the file 'modbus-gemini-1.5-flash.ksy' is correctly placed.  
# Check file permissions and ensure kaitai-struct-compiler is in your PATH.

# Example: A simple Modbus packet
meta:
  id: modbus_packet
  endian: be

seq:
  - id: header
    type: u2
  - id: data
    type: u1
    repeat: expr
    repeat-expr: header
