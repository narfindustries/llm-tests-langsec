meta:
  id: zip
  title: ZIP archive
  file-extension: zip
  xref:
    wikipedia: https://en.wikipedia.org/wiki/ZIP_(file_format)
  endian: le

enums:
  compression:
    0: none
    1: shrunk
    2: reduced_1
    3: reduced_2
    4: reduced_3
    5: reduced_4
    6: imploded
    7: tokenized
    8: deflated
    9: enhanced_deflated
    10: pkware_dcl_imploded
    12: bzip2
    14: lzma
    18: ibm_terse
    19: ibm_lz77_z
    97: wavpack
    98: ppmd

  extras:
    0x0001: zip64
    0x0007: av_info
    0x0009: os2
    0x000a: ntfs
    0x000c: openvms
    0x000d: unix
    0x000e: reserved_for_futures
    0x000f: reserved_future
    0x0014: patched_data
    0x0015: ms_packaged
    0x0065: ibm_s390_unix
    0x0066: ibm_s390_windows
    0x4690: poszip_4690

seq:
  - id: sections
    type: section
    repeat: until
    repeat-until: _.magic == 0x06054b50

types:
  section:
    seq:
      - id: magic
        type: u4
      - id: body
        size-eos: true
        type:
          switch-on: magic
          cases:
            0x04034b50: pk_section
            0x02014b50: central_dir_entry
            0x06054b50: end_of_central_dir

  pk_section:
    seq:
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
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
      - id: filename_len
        type: u2
      - id: extra_len
        type: u2
      - id: filename
        type: str
        encoding: utf-8
        size: filename_len
      - id: extra
        size: extra_len

  central_dir_entry:
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
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
      - id: filename_len
        type: u2
      - id: extra_len
        type: u2
      - id: comment_len
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_file_attrs
        type: u2
      - id: external_file_attrs
        type: u4
      - id: local_header_offset
        type: u4
      - id: filename
        type: str
        encoding: utf-8
        size: filename_len
      - id: extra
        size: extra_len
      - id: comment
        type: str
        encoding: utf-8
        size: comment_len

  end_of_central_dir:
    seq:
      - id: disk_number
        type: u2
      - id: central_dir_disk_number
        type: u2
      - id: num_central_dir_entries_disk
        type: u2
      - id: total_central_dir_entries
        type: u2
      - id: central_dir_size
        type: u4
      - id: central_dir_offset
        type: u4
      - id: comment_len
        type: u2
      - id: comment
        type: str
        encoding: utf-8
        size: comment_len
