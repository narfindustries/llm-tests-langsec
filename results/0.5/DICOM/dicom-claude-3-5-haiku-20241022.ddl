struct DicomTag {
  group: u16;
  element: u16;
}

enum VR {
  AE, AS, AT, CS, DA, DS, DT, 
  FL, FD, IS, LO, LT, PN, 
  SH, SL, SQ, SS, ST, TM, 
  UI, UL, US, UT
}

struct DicomElement {
  tag: DicomTag;
  vr: VR;
  length: u32;
  value: bytes;
}

struct PatientModule {
  name: optional<string>;
  id: optional<string>;
  birth_date: optional<string>;
  sex: optional<string>;
  age: optional<string>;
}

struct StudyModule {
  instance_uid: string;
  date: optional<string>;
  time: optional<string>;
  accession_number: optional<string>;
  referring_physician_name: optional<string>;
}

struct SeriesModule {
  modality: string;
  instance_uid: string;
  number: optional<u16>;
  description: optional<string>;
}

struct ImageModule {
  sop_class_uid: string;
  sop_instance_uid: string;
  image_type: optional<list<string>>;
  pixel_data: bytes;
}

struct DicomFile {
  preamble: bytes[128];
  dicom_prefix: bytes[4];
  meta_header: list<DicomElement>;
  patient: PatientModule;
  study: StudyModule;
  series: SeriesModule;
  image: ImageModule;
  additional_elements: list<DicomElement>;
}