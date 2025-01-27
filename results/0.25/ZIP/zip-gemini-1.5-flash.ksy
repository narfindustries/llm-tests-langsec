# This is a sample Kaitai Struct definition.  You'll need to replace this
# with your actual structure.  The error message suggests a problem with
# the ksy file itself, not necessarily the YAML.  The error code 2 often
# indicates a syntax error in the KSY file.  Carefully review your
# `zip-gemini-1.5-flash.ksy` for typos, incorrect type declarations,
# or missing semicolons.

# Example:  A simple structure to illustrate the YAML format.
# Replace this with your actual structure.

type: struct
endian: be

fields:
  - id: header
    type: u4
  - id: data
    type: seq
    size: header
    type: u1
