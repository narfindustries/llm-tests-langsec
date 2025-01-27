$schema: https://kaitai.io/schema/draft-04.yaml#
id: zip-gemini-1
endian: be

seq:
  - id: header
    type: header_t
  - id: central_directory
    type: seq
    repeat: expr
    expr: header.cd_count
    items:
      type: central_directory_entry_t
  - id: end_of_central_directory_record
    type: end_of_central_directory_record_t


types:
  header_t:
    seq:
      - id: signature
        type: u4
        enum:
          0x04034b50: header
      - id: disk_number
        type: u2
      - id: disk_start
        type: u2
      - id: entries_this_disk
        type: u2
      - id: entries_total
        type: u2
      - id: size_central_directory
        type: u4
      - id: offset_central_directory
        type: u4
      - id: comment_len
        type: u2
      - id: comment
        type: str
        size: comment_len


  central_directory_entry_t:
    seq:
      - id: signature
        type: u4
        enum:
          0x02014b50: central_directory_entry
      - id: version_made_by
        type: u2
      - id: version_needed
        type: u2
      - id: flags
        type: u2
      - id: compression_method
        type: u2
      - id: last_mod_time
        type: u2
      - id: last_mod_date
        type: u2
      - id: crc32
        type: u4
      - id: compressed_size
        type: u4
      - id: uncompressed_size
        type: u4
      - id: filename_len
        type: u2
      - id: extra_field_len
        type: u2
      - id: file_comment_len
        type: u2
      - id: disk_number_start
        type: u2
      - id: internal_attributes
        type: u2
      - id: external_attributes
        type: u4
      - id: local_header_offset
        type: u4
      - id: filename
        type: str
        size: filename_len
      - id: extra_field
        type: bytes
        size: extra_field_len
      - id: file_comment
        type: str
        size: file_comment_len


  end_of_central_directory_record_t:
    seq:
      - id: signature
        type: u4
        enum:
          0x06054b50: end_of_central_directory_record
      - id: disk_number
        type: u2
      - id: disk_number_with_cd
        type: u2
      - id: entries_this_disk
        type: u2
      - id: entries_total
        type: u2
      - id: size_central_directory
        type: u4
      - id: offset_central_directory
        type: u4
      - id: comment_len
        type: u2
      - id: comment
        type: str
        size: comment_len

