meta:
  id: gzip
  file-extension: gz
  endian: le
  title: GZIP file format
  license: CC0-1.0
  xref:
    rfc: 1952
    mime: application/gzip
    wikidata: Q192757

doc: |
  GZIP is a file format for file compression and decompression. GZIP format is based on the DEFLATE algorithm, which is a combination of LZ77 and Huffman coding.

seq:
  - id: magic
    contents: [0x1f, 0x8b]

  - id: compression_method
    type: u1
    enum: compression_methods
    doc: Compression method used (only DEFLATE is supported).

  - id: flags
    type: flags

  - id: mod_time
    type: u4
    doc: Modification time of the original file.

  - id: extra_flags
    type: u1
    enum: extra_flags
    doc: Extra flags, depending on compression method.

  - id: os
    type: u1
    enum: operating_systems
    doc: Operating system on which the compression was performed.

  - id: extras
    type: extras
    if: flags.fextra

  - id: name
    type: strz
    encoding: UTF-8
    if: flags.fname

  - id: comment
    type: strz
    encoding: UTF-8
    if: flags.fcomment

  - id: header_crc16
    type: u2
    if: flags.fhcrc

  - id: compressed_data
    size-eos: true
    doc: The compressed data.

  - id: crc32
    type: u4
    doc: CRC32 checksum of the uncompressed data.

  - id: isize
    type: u4
    doc: Size of the original (uncompressed) input data modulo 2^32.

types:
  flags:
    seq:
      - id: ftext
        type: b1
      - id: fhcrc
        type: b1
      - id: fextra
        type: b1
      - id: fname
        type: b1
      - id: fcomment
        type: b1
      - id: reserved
        type: b3

  extras:
    seq:
      - id: num_data
        type: u2
      - id: data
        type: u1
        repeat: expr
        repeat-expr: num_data

enums:
  compression_methods:
    8: deflate

  extra_flags:
    2: max_compression
    4: fast_compression

  operating_systems:
    0: fat
    1: amiga
    2: vms
    3: unix
    4: vm_cms
    5: atari_tos
    6: hpfs
    7: macintosh
    8: z_system
    9: cp_m
    10: tops_20
    11: ntfs
    12: qdos
    13: acorn_riscos
    255: unknown