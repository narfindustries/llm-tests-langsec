meta:
  id: dicom
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
    doc: 'DICOM file preamble (128 bytes)'
  
  - id: magic
    type: str
    size: 4
    encoding: ASCII
    doc: 'DICOM magic number ("DICM")'

  - id: file_meta_info_set
    type: meta_info_sequence
    doc: 'File Meta Information Set'

types:
  meta_info_sequence:
    seq:
      - id: group_length
        type: u2
        doc: 'Group 0002 Length'
      
      - id: transfer_syntax
        type: tag_element
        doc: 'Transfer Syntax UID'

      - id: implementation_uid
        type: tag_element
        doc: 'Implementation Class UID'

  tag_element:
    seq:
      - id: tag
        type: u4
        doc: 'DICOM tag identifier'
      
      - id: vr
        type: str
        size: 2
        encoding: ASCII
        doc: 'Value Representation'
      
      - id: length
        type: u2
        doc: 'Length of value'
      
      - id: value
        size: length
        type: str
        encoding: ASCII
        doc: 'Element value'