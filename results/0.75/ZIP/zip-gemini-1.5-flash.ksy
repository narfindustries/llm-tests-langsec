# This is a sample Kaitai Struct definition.  Adjust as needed for your actual data.
# The error message suggests a problem with the compilation command, not necessarily the .ksy file itself.
#  Ensure the kaitai-struct-compiler is correctly installed and accessible in your PATH.
# Also, check the paths specified in the compilation command match your file system.


meta:
  id: zip-gemini-1
  endian: be

seq:
  - id: header
    type: seq
    contents:
      - id: signature
        type: u4
        enum:
          0x04034b50: "local file header"
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_time
        type: u2
      - id: last_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: filename_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: filename
        type: str
        size: filename_length
      - id: extra_field
        type: bytes
        size: extra_field_length
      - id: file_data
        type: bytes
        size: compressed_size

