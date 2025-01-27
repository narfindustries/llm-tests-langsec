meta:
  id: zip
  file-extension: zip
  endian: le
  title: Zip Archive (GPT-4 Enhanced Version)
  license: CC0-1.0
  ks-version: 0.9

doc: |
  ZIP is a popular archive file format that supports lossless data compression.
  A ZIP file may contain one or more files or directories that may have been compressed.
  The ZIP file format permits a number of compression algorithms.

seq:
  - id: sections
    type: section
    repeat: eos

types:
  section:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]  # "PK\x03\x04"
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: compression
        type: u2
      - id: mod_time
        type: u2
      - id: mod_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: file_name_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_name
        type: str
        encoding: UTF-8
        size: file_name_length
      - id: extra_field
        size: extra_field_length
      - id: body
        size: compressed_size
        if: compressed_size > 0