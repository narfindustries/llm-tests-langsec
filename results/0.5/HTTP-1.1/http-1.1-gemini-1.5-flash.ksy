$schema: https://kaitai.io/schema/draft-04.yaml
id: http-1.1-gemini-1.5-flash
endian: be

seq:
  - id: magic
    type: u4
  - id: version
    type: u2
  - id: flags
    type: u2
  - id: length
    type: u4
  - id: payload
    type: bytes
    size: length
