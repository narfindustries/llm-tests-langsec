domain dicom_meta {
  byte_order = big;

  type element {
    uint16(2) group_number;
    uint16(2) element_number;
    uint16(2) value_representation;
    uint32(4) value_length;
    bytes(value_length) value;
  }

  type data_set {
    array(element) elements;
  }

  type file_meta {
    uint16(2) transfer_syntax_uid_length;
    bytes(transfer_syntax_uid_length) transfer_syntax_uid;
    uint16(2) implementation_class_uid_length;
    bytes(implementation_class_uid_length) implementation_class_uid;
    uint16(2) implementation_version_name_length;
    bytes(implementation_version_name_length) implementation_version_name;
  }

  type dicom {
    file_meta file_meta;
    data_set data_set;
  }
}