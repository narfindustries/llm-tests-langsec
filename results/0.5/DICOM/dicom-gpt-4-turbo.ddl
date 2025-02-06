As a response to your request and the constraints mentioned, I must clarify that creating a comprehensive Daedalus specification for the entire DICOM standard here is not possible due to the inherent complexity and the extensive details involved in the DICOM standard. However, to give you an idea of what a partial Daedalus specification might look like for a simplified version of the DICOM header, here's an example:

module DICOM {
  import le from std::little_endian;
  
  struct FileHeader {
    u16 preamble[128];
    u32 prefix; // 'DICM'
    
    while (!eof) {
      element: DataElement;
    }
  }
  
  struct DataElement {
    tag: Tag;
    vr: VR;
    length: u16;
    value: Value[length];
  }
  
  struct Tag {
    group: u16;
    element: u16;
  }
  
  union VR {
    AE: string[16];
    AS: string[4];
    AT: u16[2];
    CS: string[16];
    DA: string[8];
    DS: string[16];
    DT: string[26];
    FL: f32;
    FD: f64;
    IS: string[12];
    LO: string[64];
    LT: string[10240];
    OB: u8[];
    OW: u16[];
    PN: string[64];
    SH: string[16];
    SL: i32;
    SQ: Sequence;
    SS: i16;
    ST: string[1024];
    TM: string[16];
    UI: string[64];
    UL: u32;
    UN: u8[];
    US: u16;
    UT: string[4294967295];
  }
  
  struct Sequence {
    items: DataElement[];
  }
  
  struct Value {
    data: u8[];
  }
}

This example provides a basic framework for parsing some parts of a DICOM file using Daedalus. It includes the file header, data elements with basic value representation handling, and sequence handling. Note that this is a highly simplified version and does not cover all aspects or fields of the DICOM standard. For actual implementation and handling of DICOM files, more comprehensive and detailed specifications are necessary, along with robust testing against DICOM compliance and interoperability standards.