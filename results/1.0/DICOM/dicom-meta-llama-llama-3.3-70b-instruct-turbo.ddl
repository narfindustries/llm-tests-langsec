domain DICOM {
  // Define a subset of the DICOM protocol
  structure DICOM_META {
    uint16(16) group_number;
    uint16(16) element_number;
    uint16(16) value_representation;
    uint16(16) LengthOfValue;
    bytes(LengthOfValue) value_field;
  }

  // Define a primary item in the DICOM data set
  structure DICOM_DATA_SET {
    sequence[DICOM_META] data_elements;
  }

  // Structure to represent the full DICOM meta information header
  structure DICOM.Persistence {
    DICOM_DATA_SET header;
  }
}