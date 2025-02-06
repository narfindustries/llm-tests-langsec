format:DDL
version:1.0

type uint16 = uint16
type uint32 = uint32
type int16 = int16
type int32 = int32
type string = string
type bytes = bytes
type date = date
type time = time

type DataElement {
  uint16 tag
  string(2) vr
  Value value
}

type Value {
  switch(vr) {
    case "OB":
      bytes value
      break
    case "OD":
    case "OF":
      double value
      break
    case "OL":
      uint32 value
      break
    case "OW":
      uint16 value
      break
    case "UI":
      uint32 value
      break
    case "UR":
      string value
      break
    case "US":
      uint16 value
      break
    case "SL":
      int32 value
      break
    case "UL":
      uint32 value
      break
    case "FL":
      float value
      break
    case "FD":
      double value
      break
    case "FE":
      float value
      break
    case "SS":
      int16 value
      break
    case "UST":
      string value
      break
  }
}

type MetaHeader {
  uint16 tag
  uint16 group
  uint16 element
  uint32 length
  string(2) vr
  string(length) value
}

type DataSet {
  array DataElement dataElements
}

type DICOM {
  bytes(128) preamble
  string(4) prefix
  MetaHeader meta
  DataSet data
}

structure DICOM_file {
  bytes input
  DICOM output
  rule DICOM_file_rule
}

rule DICOM_file_rule {
  sequence {
    string(4) magic = "DICM"
    bytes(128) preamble
    string(4) prefix = "DICM"
    MetaHeader meta
    DataSet data
  }
}

schema DICOM_schema {
  structure DICOM_file
}

type PatientName {
  string familyName
  string givenName
  string middleName
  string namePrefix
  string nameSuffix
}

type PatientID {
  string patientID
}

type StudyDate {
  date studyDate
}

type StudyTime {
  time studyTime
}

type Modality {
  string modality
}

type BodyPart {
  string bodyPart
}

type PixelData {
  bytes pixelData
}

type PatientSex {
  string patientSex
}

type Laterality {
  string laterality
}

type PatientAge {
  string patientAge
}

structure PatientName_field {
  string(64) patientName
}

structure PatientID_field {
  string(64) patientID
}

structure StudyDate_field {
  date studyDate
}

structure StudyTime_field {
  time studyTime
}

structure Modality_field {
  string(16) modality
}

structure BodyPart_field {
  string(16) bodyPart
}

structure PixelData_field {
  bytes pixelData
}

structure PatientSex_field {
  string(16) patientSex
}

structure Laterality_field {
  string(16) laterality
}

structure PatientAge_field {
  string(16) patientAge
}

rule PatientName_rule {
  string(64) patientName
}

rule PatientID_rule {
  string(64) patientID
}

rule StudyDate_rule {
  date studyDate
}

rule StudyTime_rule {
  time studyTime
}

rule Modality_rule {
  string(16) modality
}

rule BodyPart_rule {
  string(16) bodyPart
}

rule PixelData_rule {
  bytes pixelData
}

rule PatientSex_rule {
  string(16) patientSex
}

rule Laterality_rule {
  string(16) laterality
}

rule PatientAge_rule {
  string(16) patientAge
}

schema PatientName_schema {
  structure PatientName_field
}

schema PatientID_schema {
  structure PatientID_field
}

schema StudyDate_schema {
  structure StudyDate_field
}

schema StudyTime_schema {
  structure StudyTime_field
}

schema Modality_schema {
  structure Modality_field
}

schema BodyPart_schema {
  structure BodyPart_field
}

schema PixelData_schema {
  structure PixelData_field
}

schema PatientSex_schema {
  structure PatientSex_field
}

schema Laterality_schema {
  structure Laterality_field
}

schema PatientAge_schema {
  structure PatientAge_field
}