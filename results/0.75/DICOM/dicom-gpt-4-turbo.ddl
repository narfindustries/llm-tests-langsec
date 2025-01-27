module DICOM {

  -- Define the basic data types
  type UINT8 = UInt<8>;
  type UINT16 = UInt<16>;
  type UINT32 = UInt<32>;
  type STRING = Bytes<Z>;

  -- Define a structure for the DICOM Header
  struct DICOMHeader {
    preamble: Bytes<128>;
    prefix: STRING(4);

    -- The function to check DICOM prefix
    fn is_dicom_prefix(s: STRING) -> Bool {
      s == "DICM"
    }
  }

  -- DICOM data element definition
  struct DataElement {
    tag_group: UINT16;
    tag_element: UINT16;
    vr: STRING(2);
    length: UINT32;
    value: Bytes<this.length>;
  }

  -- Root structure for DICOM file parsing
  struct DICOMFile {
    header: DICOMHeader;
    elements: List<DataElement>;

    -- Function to parse elements
    fn parse_elements(stream: Stream) -> List<DataElement> {
      var elements: List<DataElement> = [];
      while(!stream.eof()) {
        elements.push(DataElement::parse(stream));
      }
      return elements;
    }
  }

  -- Main entry point to parse DICOM file
  fn parse_dicom(stream: Stream) -> DICOMFile {
    var header = DICOMHeader::parse(stream);
    assert(header.is_dicom_prefix(header.prefix), "Not a valid DICOM file");

    var elements = DICOMFile::parse_elements(stream);
    return DICOMFile { header, elements };
  }
}