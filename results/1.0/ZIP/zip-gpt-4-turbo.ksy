meta:
  id: zip
  file-extension: zip
  endian: le
  title: ZIP Archive
  license: CC0-1.0
  ks-version: 0.9
doc: |
  ZIP is a popular archive file format that is used to compress collections of files.
  It optionally supports several kinds of compression and digital signing and is used
  by numerous applications including as a base format for other archive formats.
doc-ref: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
seq:
  - id: sections
    type: section
    repeat: eos
types:
  section:
    seq:
      - id: signature
        type: u4
        valid:
          eq: 0x04034b50
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: compression
        type: u2
      - id: file_mod_time
        type: u2
      - id: file_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: file_name_len
        type: u2
      - id: extra_field_len
        type: u2
      - id: file_name
        type: str
        encoding: UTF-8
        size: file_name_len
      - id: extra_field
        size: extra_field_len