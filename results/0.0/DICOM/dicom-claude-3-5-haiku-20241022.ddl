type DicomVR =
  | AE
  | AS
  | AT
  | CS
  | DA
  | DS
  | DT
  | FL
  | FD
  | IS
  | LO
  | LT
  | PN
  | SH
  | SL
  | SS
  | ST
  | TM
  | UI
  | UL
  | US

type DicomAttribute = {
  tag: uint32,
  vr: DicomVR,
  value: bytes
}

type PatientModule = {
  patient_name: option<string>,
  patient_id: option<string>,
  patient_birth_date: option<string>,
  patient_sex: option<string>,
  patient_age: option<string>,
  patient_weight: option<float>
}

type StudyModule = {
  study_instance_uid: string,
  study_date: option<string>,
  study_time: option<string>,
  accession_number: option<string>,
  study_description: option<string>,
  study_id: option<string>
}

type SeriesModule = {
  modality: string,
  series_number: option<int>,
  series_description: option<string>,
  series_instance_uid: string,
  series_date: option<string>,
  series_time: option<string>
}

type ImageModule = {
  sop_class_uid: string,
  sop_instance_uid: string,
  image_type: list<string>,
  pixel_data: bytes,
  rows: uint16,
  columns: uint16,
  bits_allocated: uint16,
  bits_stored: uint16,
  high_bit: uint16,
  pixel_representation: uint16
}

type DicomFile = {
  preamble: bytes<128>,
  magic_number: bytes<4>,
  attributes: list<DicomAttribute>,
  patient: PatientModule,
  study: StudyModule,
  series: SeriesModule,
  image: ImageModule
}

let parse_dicom_vr = fun input ->
  match input with
  | "AE" -> AE
  | "AS" -> AS
  | "AT" -> AT
  | "CS" -> CS
  | "DA" -> DA
  | "DS" -> DS
  | "DT" -> DT
  | "FL" -> FL
  | "FD" -> FD
  | "IS" -> IS
  | "LO" -> LO
  | "LT" -> LT
  | "PN" -> PN
  | "SH" -> SH
  | "SL" -> SL
  | "SS" -> SS
  | "ST" -> ST
  | "TM" -> TM
  | "UI" -> UI
  | "UL" -> UL
  | "US" -> US

let parse_dicom_attribute = fun input ->
  let tag = parse_uint32 input in
  let vr = parse_dicom_vr input in
  let value = parse_bytes input in
  { tag, vr, value }

let parse_patient_module = fun input ->
  {
    patient_name: optional parse_string input,
    patient_id: optional parse_string input,
    patient_birth_date: optional parse_string input,
    patient_sex: optional parse_string input,
    patient_age: optional parse_string input,
    patient_weight: optional parse_float input
  }

let parse_study_module = fun input ->
  {
    study_instance_uid: parse_string input,
    study_date: optional parse_string input,
    study_time: optional parse_string input,
    accession_number: optional parse_string input,
    study_description: optional parse_string input,
    study_id: optional parse_string input
  }

let parse_series_module = fun input ->
  {
    modality: parse_string input,
    series_number: optional parse_int input,
    series_description: optional parse_string input,
    series_instance_uid: parse_string input,
    series_date: optional parse_string input,
    series_time: optional parse_string input
  }

let parse_image_module = fun input ->
  {
    sop_class_uid: parse_string input,
    sop_instance_uid: parse_string input,
    image_type: many parse_string input,
    pixel_data: parse_bytes input,
    rows: parse_uint16 input,
    columns: parse_uint16 input,
    bits_allocated: parse_uint16 input,
    bits_stored: parse_uint16 input,
    high_bit: parse_uint16 input,
    pixel_representation: parse_uint16 input
  }

let parse_dicom = fun input ->
  let preamble = take 128 input in
  let magic_number = take 4 input in
  let _ = assert (magic_number = "DICM") in
  let attributes = many parse_dicom_attribute input in
  let patient = parse_patient_module input in
  let study = parse_study_module input in
  let series = parse_series_module input in
  let image = parse_image_module input in
  {
    preamble,
    magic_number,
    attributes,
    patient,
    study,
    series,
    image
  }