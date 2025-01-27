meta:
  id: dicom
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
  - id: prefix
    contents: [0x44, 0x49, 0x43, 0x4D]
  - id: file_meta_info_group
    type: meta_info_group
types:
  meta_info_group:
    seq:
      - id: group_length_tag
        type: tag_item
      - id: transfer_syntax_uid_tag
        type: tag_item
  tag_item:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
      - id: length
        type: u4
      - id: value
        size: length
        type: str
        encoding: UTF-8