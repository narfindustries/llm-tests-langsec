meta:
  id: dicom
  application: DICOM
  file-extension: dcm
  endian: le
seq:
  - id: preamble
    size: 128
  - id: prefix
    contents: 'DICM'
  - id: file_meta_information_group_length
    type: u4
  - id: file_meta_information_version
    type: u2
  - id: media_storage_sop_class_uid
    type: dicom_ui
  - id: media_storage_sop_instance_uid
    type: dicom_ui
  - id: transfer_syntax_uid
    type: dicom_ui
  - id: implementation_class_uid
    type: dicom_ui
  - id: implementation_version_name
    type: dicom_lo
  - id: source_application_entity_title
    type: dicom_ae
  - id: sending_application_entity_title
    type: dicom_ae
  - id: receiving_application_entity_title
    type: dicom_ae
  - id: private_information_creator_uid
    type: dicom_ui
  - id: private_information
    type: dicom_ob
  - id: elements
    type: element
    repeat: eos
types:
  element:
    seq:
      - id: tag
        type: tag
      - id: vr
        type: dicom_vr
      - id: length
        type: u2
      - id: value
        size: length
  tag:
    seq:
      - id: group
        type: u2
      - id: element
        type: u2
  dicom_vr:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size: 2
  dicom_ui:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  dicom_lo:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  dicom_ae:
    seq:
      - id: value
        type: str
        encoding: ASCII
        size-eos: true
  dicom_ob:
    seq:
      - id: value
        type: u1
        repeat: eos