# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual definition.
# The error message suggests a problem with the compilation process, not necessarily the schema itself.
# Ensure the `network-time-protocol-version-4-gemini-1.5-flash.ksy` file exists and is accessible.
# Also check that the kaitai-struct-compiler is correctly installed and in your PATH.

# Example:  A simple structure for demonstration purposes.  Replace with your NTPv4 definition.
type: struct
endian: be
fields:
  - id: magic
    type: u4
  - id: version
    type: u1
  - id: leap
    type: u1
  - id: mode
    type: u1
  - id: stratum
    type: u1
  - id: poll
    type: u1
  - id: precision
    type: u1
  - id: root_delay
    type: u4
  - id: root_dispersion
    type: u4
  - id: ref_id
    type: u4
  - id: ref_timestamp
    type: u8
  - id: orig_timestamp
    type: u8
  - id: recv_timestamp
    type: u8
  - id: xmit_timestamp
    type: u8

