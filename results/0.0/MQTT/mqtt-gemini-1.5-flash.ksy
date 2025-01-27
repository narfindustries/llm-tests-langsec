# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual schema.
# The error message suggests a problem with the compilation command, not necessarily the schema itself.
#  The error code 2 often indicates a problem with the input file (mqtt-gemini-1.5-flash.ksy).
#  Ensure this file exists and is correctly formatted.  Common issues include:
#   - Syntax errors in the KSY file.
#   - Incorrect use of types or keywords.
#   - References to non-existent types or enums.
#   - Issues with the file encoding (should be UTF-8).

# Check your mqtt-gemini-1.5-flash.ksy file carefully for errors.

type: struct
id: mqtt_gemini_1_5_flash

endian: be

seq:
  - id: header
    type:
      seq:
        - id: magic
          type: u4
        - id: version
          type: u2
        - id: length
          type: u4
  - id: data
    type: bytes
    size: header.length

