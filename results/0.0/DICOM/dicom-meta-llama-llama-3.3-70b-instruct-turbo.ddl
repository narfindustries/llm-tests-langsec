format = dicom
version = 3.0

 PatientName : string(64, "UTF-8")
 PatientID : string(64, "UTF-8")
 StudyInstanceUID : string(64, "UTF-8")
 SeriesInstanceUID : string(64, "UTF-8")
 SOPInstanceUID : string(64, "UTF-8")

 IntegerString : string(pattern = "[0-9]+")
 CodeString : string(pattern = "[0-9A-Z]+")
 Date : string(pattern = "[0-9]{4}[0-9]{2}[0-9]{2}")
 Time : string(pattern = "[0-9]{2}[0-9]{2}[0-9]{2}")
 DateTime : string(pattern = "[0-9]{4}[0-9]{2}[0-9]{2}[0-9]{2}[0-9]{2}[0-9]{2}")

enum PatientSex {
  M, F, O
}

enum Modality {
  CT, MR, US, NM, MG, DX, RF, OT
}

enum PatientPosition {
  HFP, HFS, HFDR, FFDR, FFDL, FFLR, FHDR, HFDL, HFLR
}

struct PatientModule {
  PatientName: PatientName
  PatientID: PatientID
  PatientSex: PatientSex
  PatientBirthDate: Date
  PatientAge: string(4, "UTF-8")
  PatientSize: string(4, "UTF-8")
  PatientWeight: string(4, "UTF-8")
}

struct StudyModule {
  StudyInstanceUID: StudyInstanceUID
  StudyDate: string(8, "UTF-8")
  StudyTime: string(6, "UTF-8")
  StudyDescription: string(64, "UTF-8")
  Modality: Modality
  PatientPosition: PatientPosition
}

struct SeriesModule {
  SeriesInstanceUID: SeriesInstanceUID
  SeriesDate: string(8, "UTF-8")
  SeriesTime: string(6, "UTF-8")
  SeriesDescription: string(64, "UTF-8")
  Modality: Modality
  PatientPosition: PatientPosition
}

struct SOPModule {
  SOPInstanceUID: SOPInstanceUID
  SOPClassUID: string(64, "UTF-8")
  SOPInstanceUID: string(64, "UTF-8")
}

struct FileMetaInformation {
  FileMetaInformationVersion: string(2, "UTF-8")
  TransferSyntaxUID: string(64, "UTF-8")
  ImplementationClassUID: string(64, "UTF-8")
  SourceApplicationEntityTitle: string(16, "UTF-8")
}

struct TransferSyntax {
  TransferSyntaxUID: string(64, "UTF-8")
}

struct ImplementationClass {
  ImplementationClassUID: string(64, "UTF-8")
}

struct MediaStorage {
  MediaStorageSOPInstanceUID: string(64, "UTF-8")
  MediaStorageSOPClassUID: string(64, "UTF-8")
}

struct PixelData {
  PixelDataGroup: uint16[2]
  PixelDataElement: uint16[2]
  PixelDataLength: uint32
  PixelData: bytes(PixelDataLength)
}

struct FrameOfReference {
  FrameOfReferenceUID: string(64, "UTF-8")
}

struct PlaneOrientation {
  ImageOrientationPatient: float[3]
  ImagePositionPatient: float[3]
}

struct PlanePosition {
  ImagePositionPatient: float[3]
}

struct PixelMeasures {
  PixelSpacing: float[2]
  SliceThickness: float
}

struct ImagePixel {
  PixelDataGroup: uint16[2]
  PixelDataElement: uint16[2]
  PixelDataLength: uint32
  PixelData: bytes(PixelDataLength)
}

struct Image {
  SOPInstanceUID: string(64, "UTF-8")
  SOPClassUID: string(64, "UTF-8")
  Rows: uint16
  Columns: uint16
  BitsAllocated: uint16
  BitsStored: uint16
  HighBit: uint16
  PixelRepresentation: uint16
  PixelMeasures: PixelMeasures
  PlaneOrientation: PlaneOrientation
  PlanePosition: PlanePosition
  ImagePixel: ImagePixel
}

struct DICOMFile {
  FileMetaInformation: FileMetaInformation
  PatientModule: PatientModule
  StudyModule: StudyModule
  SeriesModule: SeriesModule
  SOPModule: SOPModule
  TransferSyntax: TransferSyntax
  ImplementationClass: ImplementationClass
  MediaStorage: MediaStorage
  FrameOfReference: FrameOfReference
  Image: Image
}