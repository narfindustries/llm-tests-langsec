types:
  dicom_file:
    seq:
      - id: file_meta_information_group_length
        type: u4
      - id: file_meta_information_version
        type: u2
      - id: media_storage_sop_instance_uid
        type: str
      - id: media_storage_sop_class_uid
        type: str
      - id: transfer_syntax_uid
        type: str
      - id: implementation_class_uid
        type: str
      - id: implementation_version_name
        type: str
      - id: source_application_entity_title
        type: str
      - id: private_information_creator_uid
        type: str
      - id: private_information
        type: bytes
      - id: dataset
        type: dicom_dataset

  dicom_dataset:
    seq:
      - id: elements
        type: dicom_element
        repeat: true

  dicom_element:
    seq:
      - id: tag
        type: u4
      - id: vr
        type: str
      - id: vl
        type: u4
      - id: value
        type: value

  value:
    switch: vr
    cases:
      - "AE":
        type: str
      - "AS":
        type: str
      - "AT":
        type: u4
      - "CS":
        type: str
      - "DA":
        type: str
      - "DS":
        type: f4
      - "DT":
        type: str
      - "FL":
        type: f4
      - "FD":
        type: f8
      - "IS":
        type: i4
      - "LO":
        type: str
      - "LT":
        type: str
      - "OB":
        type: bytes
      - "OD":
        type: bytes
      - "OF":
        type: bytes
      - "OW":
        type: bytes
      - "PN":
        type: str
      - "SH":
        type: str
      - "SL":
        type: i4
      - "SQ":
        type: dicom_sequence
      - "SS":
        type: i2
      - "ST":
        type: str
      - "TM":
        type: str
      - "UI":
        type: str
      - "UL":
        type: u4
      - "UN":
        type: bytes
      - "US":
        type: u2
      - "UT":
        type: str
      - _:
        type: bytes


  dicom_sequence:
    seq:
      - id: items
        type: dicom_dataset
        repeat: true

