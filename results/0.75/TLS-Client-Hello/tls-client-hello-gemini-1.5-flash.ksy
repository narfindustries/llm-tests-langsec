# This is a sample Kaitai Struct definition.  You'll need to replace this with your actual definition.  The error message suggests a problem with the compilation command, not necessarily the structure itself.  The path names in the error message are also likely specific to your environment and will need adjustment.

#  The error is likely due to issues with the .ksy file itself (e.g., syntax errors, unsupported features) or problems with the kaitai-struct-compiler installation/environment.  Review the .ksy file carefully.


$schema: "http://kaitai.io/v1.0/ksy"

meta:
  id: tls-client-hello-gemini-1
  title: TLS Client Hello (Gemini-1.5 Flash)


types:
  uint16_le:
    seq:
      - id: value
        type: u2le

  uint32_le:
    seq:
      - id: value
        type: u4le

  uint64_le:
    seq:
      - id: value
        type: u8le


seq:
  - id: record_layer
    type: record_layer_type

types:
  record_layer_type:
    seq:
      - id: content_type
        type: u1
      - id: version
        type: version_type
      - id: length
        type: uint16_le
      - id: fragment
        type: fragment_type


  version_type:
    seq:
      - id: major
        type: u1
      - id: minor
        type: u1

  fragment_type:
    type: bytes
    size: (lambda x: x.length)


