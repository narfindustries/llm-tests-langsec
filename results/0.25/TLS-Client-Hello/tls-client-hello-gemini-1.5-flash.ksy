# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual schema.
# The error message suggests a problem with the compilation command, not necessarily the schema itself.
#  The path 'generated/999999/0.25/TLS-Client-Hello/tls-client-hello-gemini-1.5-flash.ksy' indicates a file structure issue.
#  Ensure the file exists at the specified location and is correctly formatted.

# Example:  A simple structure
type: struct
endian: be
seq:
  - id: magic
    type: u4
  - id: version
    type: u2
  - id: data
    type: str
    len: 10

