meta:
  id: gzip_deepseek_chat
  file-extension: gz
  endian: le
  encoding: UTF-8
  license: MIT
  ks-version: 0.9

seq:
  - id: magic
    contents: [0x1F, 0x8B]
    doc: Magic number identifying the file as GZIP
  - id: compression_method
    type: u1
    doc: Compression method used (8 = DEFLATE)
  - id: flags
    type: u1
    doc: Flags indicating additional fields and options
  - id: modification_time
    type: u4
    doc: Modification time of the original file (UNIX timestamp)
  - id: extra_flags
    type: u1
    doc: Additional flags for compression
  - id: os_type
    type: u1
    doc: Operating system type that created the file
  - id: extra_field
    type: extra_field
    if: 'flags & 0x04 != 0'
    doc: Optional extra field
  - id: original_filename
    type: strz
    encoding: UTF-8
    if: 'flags & 0x08 != 0'
    doc: Original filename (null-terminated)
  - id: comment
    type: strz
    encoding: UTF-8
    if: 'flags & 0x10 != 0'
    doc: Comment (null-terminated)
  - id: header_crc
    type: u2
    if: 'flags & 0x02 != 0'
    doc: CRC16 checksum of the header
  - id: compressed_data
    type: compressed_data
    doc: Compressed data blocks
  - id: crc32
    type: u4
    doc: CRC32 checksum of the uncompressed data
  - id: uncompressed_size
    type: u4
    doc: Size of the uncompressed data

types:
  extra_field:
    seq:
      - id: length
        type: u2
        doc: Length of the extra field
      - id: data
        size: length
        doc: Extra field data

  compressed_data:
    seq:
      - id: blocks
        type: deflate_block
        repeat: eos
        doc: Compressed data blocks

  deflate_block:
    seq:
      - id: block_header
        type: u1
        doc: Block header
      - id: block_data
        size-eos: true
        doc: Block data