# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual schema.
# The error message suggests a problem with the compilation process, not necessarily the schema itself.
# The error code 2 often indicates a problem with the input file (nitf-gemini-1.5-flash.ksy).
#  Ensure that file exists and is correctly formatted.

#  Consider adding more robust error handling to your compilation script.

type: struct
endian: be

seq:
  - id: header
    type: seq
    items:
      - id: file_header
        type: str
        size: 8
      - id: header_length
        type: u4
      - id: file_type
        type: str
        size: 8

  - id: data
    type: str
    size: (header.header_length - 16)


