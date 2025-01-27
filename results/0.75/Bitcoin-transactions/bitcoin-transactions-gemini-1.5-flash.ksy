# This is a sample Kaitai Struct definition.  You will need to replace this with your actual schema.
# The error message suggests a problem with the compilation command, not the schema itself.
# The issue is likely in the pathing or the ksy file itself, not this YAML.


# Example:  A simple structure
#  This is NOT related to the Bitcoin transaction, but a simple example for testing

type: struct
endian: be

fields:
  - id: magic
    type: u4
  - id: version
    type: u4
  - id: payload_length
    type: u4
  - id: payload
    type: bytes
    size: payload_length
  - id: checksum
    type: u4

