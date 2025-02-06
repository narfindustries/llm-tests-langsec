dicomFile = DicomHeader : DicomElements

DicomHeader = {
  fileMetaInformationGroupLength: uint32,
  fileMetaInformationVersion: [uint16, uint16],
  mediaStorageSOPClassUID: UID,
  mediaStorageSOPInstanceUID: UID,
  transferSyntaxUID: UID,
  implementationClassUID: UID,
  implementationVersionName: string
}

DicomElements = [DicomElement]

DicomElement = {
  tag: [uint16, uint16],
  vr: VR,
  length: uint32,
  value: Value
}

VR = "AE" | "AS" | "AT" | "CS" | "DA" | "DT" | "FL" | "FD" | "IS" | "LO" | "LT" | "OB" | "OD" | "OF" | "OW" | "PN" | "SH" | "SL" | "SQ" | "SS" | "ST" | "TM" | "UI" | "UL" | "UN" | "US" | "UT"

Value = string -- This is a massive simplification.  A real implementation would require many more types and nested structures to handle the various VRs and complex data structures within DICOM. Sequences (SQ) would require recursive definitions.  Many VRs would need dedicated types.

UID = string
