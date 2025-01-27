meta:
  id: zip-gemini-1
  endian: be

types:
  header:
    seq:
      - id: signature
        type: u4
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: method
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: filename_length
        type: u2
      - id: extra_length
        type: u2
  file_entry:
    seq:
      - id: header
        type: header
      - id: filename
        type: str
        size: lambda: this.header.filename_length
      - id: extra
        type: bytes
        size: lambda: this.header.extra_length
      - id: compressed_data
        type: bytes
        size: lambda: this.header.compressed_size
      - id: uncompressed_data
        type: bytes
        size: lambda: this.header.uncompressed_size


seq:
  - id: entries
    type: file_entry+

