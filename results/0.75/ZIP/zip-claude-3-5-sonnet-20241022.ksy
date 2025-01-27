meta:
  id: zip
  title: ZIP archive format
  file-extension: zip
  xref:
    iso: 21320-1
    justsolve: ZIP
    pronom: x-fmt/263
    wikidata: Q136218
  license: CC0-1.0
  endian: le

seq:
  - id: sections
    type: pk_section
    repeat: eos

types:
  pk_section:
    seq:
      - id: magic
        contents: 'PK'
      - id: section_type
        type: u2
      - id: body
        type:
          switch-on: section_type
          cases:
            0x0403: central_dir_entry
            0x0201: local_file
            0x0605: end_of_central_dir
            0x0606: zip64_end_of_central_dir
            0x0706: zip64_end_of_central_dir_locator

  local_file:
    seq:
      - id: version
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
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
      - id: extra_len
        type: u2
      - id: file_name
        type: str
        size: file_name_len
        encoding: UTF-8
      - id: extra
        size: extra_len
      - id: body
        size: compressed_size

  central_dir_entry:
    seq:
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
        enum: compression
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
      - id: extra_len
        type: u2
      - id: comment_len
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_attrs
        type: u2
      - id: external_attrs
        type: u4
      - id: local_header_offset
        type: u4
      - id: file_name
        type: str
        size: file_name_len
        encoding: UTF-8
      - id: extra
        size: extra_len
      - id: comment
        type: str
        size: comment_len
        encoding: UTF-8

  end_of_central_dir:
    seq:
      - id: disk_number
        type: u2
      - id: disk_start
        type: u2
      - id: qty_central_dir_entries_on_disk
        type: u2
      - id: qty_central_dir_entries_total
        type: u2
      - id: central_dir_size
        type: u4
      - id: central_dir_offset
        type: u4
      - id: comment_len
        type: u2
      - id: comment
        type: str
        size: comment_len
        encoding: UTF-8

  zip64_end_of_central_dir:
    seq:
      - id: size_of_record
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: disk_number
        type: u4
      - id: disk_number_start
        type: u4
      - id: qty_central_dir_entries_on_disk
        type: u8
      - id: qty_central_dir_entries_total
        type: u8
      - id: central_dir_size
        type: u8
      - id: central_dir_offset
        type: u8
      - id: extensible_data
        size: size_of_record - 44

  zip64_end_of_central_dir_locator:
    seq:
      - id: disk_number_with_zip64_end_of_central_dir
        type: u4
      - id: end_of_central_dir_offset
        type: u8
      - id: number_of_disks
        type: u4

enums:
  compression:
    0: none
    1: shrunk
    2: reduced_1
    3: reduced_2
    4: reduced_3
    5: reduced_4
    6: imploded
    8: deflated
    9: enhanced_deflated
    10: pkware_dcl_imploded
    12: bzip2
    14: lzma
    18: ibm_terse
    19: ibm_lz77_z
    96: jpeg
    97: wavpack
    98: ppmd
    99: aes_encrypted