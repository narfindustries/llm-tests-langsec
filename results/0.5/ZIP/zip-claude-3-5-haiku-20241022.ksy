meta:
  id: zip_file
  file-extension: zip
  endian: le
  encoding: ascii

types:
  section:
    seq:
      - id: section_type
        type: u4
        enum: section_signatures
    instances:
      section_data:
        io: _root._io
        pos: _io.pos
        type:
          switch-on: section_type
          cases:
            section_signatures::local_file_header: local_file_header
            section_signatures::central_file_header: central_file_header
            section_signatures::end_of_central_directory: end_of_central_directory
            section_signatures::zip64_end_of_central_directory: zip64_end_of_central_directory
            section_signatures::zip64_end_of_central_directory_locator: zip64_end_of_central_directory_locator

  local_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x03, 0x04]
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
        enum: compression_methods
      - id: last_mod_file_time
        type: u2
      - id: last_mod_file_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: filename_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: filename
        type: str
        size: filename_length
        encoding: ascii
      - id: extra_field
        size: extra_field_length
      - id: body
        size: compressed_size

  central_file_header:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x01, 0x02]
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: general_purpose_bit_flag
        type: u2
      - id: compression_method
        type: u2
        enum: compression_methods
      - id: last_mod_file_time
        type: u2
      - id: last_mod_file_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: filename_length
        type: u2
      - id: extra_field_length
        type: u2
      - id: file_comment_length
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_file_attributes
        type: u2
      - id: external_file_attributes
        type: u4
      - id: relative_offset_of_local_header
        type: u4
      - id: filename
        type: str
        size: filename_length
        encoding: ascii
      - id: extra_field
        size: extra_field_length
      - id: file_comment
        type: str
        size: file_comment_length
        encoding: ascii

  end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x05, 0x06]
      - id: number_of_this_disk
        type: u2
      - id: disk_with_central_directory
        type: u2
      - id: total_entries_central_directory_this_disk
        type: u2
      - id: total_entries_central_directory
        type: u2
      - id: size_of_central_directory
        type: u4
      - id: offset_to_central_directory
        type: u4
      - id: zip_file_comment_length
        type: u2
      - id: zip_file_comment
        type: str
        size: zip_file_comment_length
        encoding: ascii

  zip64_end_of_central_directory:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x06, 0x06]
      - id: size_of_record
        type: u8
      - id: version_made_by
        type: u2
      - id: version_needed_to_extract
        type: u2
      - id: number_of_this_disk
        type: u4
      - id: disk_with_central_directory
        type: u4
      - id: total_entries_central_directory_this_disk
        type: u8
      - id: total_entries_central_directory
        type: u8
      - id: size_of_central_directory
        type: u8
      - id: offset_to_central_directory
        type: u8

  zip64_end_of_central_directory_locator:
    seq:
      - id: signature
        contents: [0x50, 0x4b, 0x06, 0x07]
      - id: number_of_disk_with_start_of_zip64_eocd
        type: u4
      - id: relative_offset_of_zip64_eocd
        type: u8
      - id: total_number_of_disks
        type: u4

seq:
  - id: sections
    type: section
    repeat: eos

enums:
  section_signatures:
    0x04034B50: local_file_header
    0x02014B50: central_file_header
    0x06054B50: end_of_central_directory
    0x06064B50: zip64_end_of_central_directory
    0x06054B60: zip64_end_of_central_directory_locator

  compression_methods:
    0: no_compression
    1: shrunk
    2: reduced_factor1
    3: reduced_factor2
    4: reduced_factor3
    5: reduced_factor4
    6: imploded
    8: deflated
    9: enhanced_deflate
    10: pkware_dcl_imploded
    12: bzip2
    14: lzma
    18: ibm_terse
    19: ibm_lz77_z
    98: blech
    99: winzip_aes