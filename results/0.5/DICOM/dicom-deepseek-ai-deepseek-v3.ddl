DICOM = {
  FileMetaInformationGroupLength: uint32,
  FileMetaInformationVersion: bytes,
  MediaStorageSOPClassUID: string,
  MediaStorageSOPInstanceUID: string,
  TransferSyntaxUID: string,
  ImplementationClassUID: string,
  ImplementationVersionName: string,
  SourceApplicationEntityTitle: string,
  SendingApplicationEntityTitle: string,
  ReceivingApplicationEntityTitle: string,
  PrivateInformationCreatorUID: string,
  PrivateInformation: bytes,
  PatientName: string,
  PatientID: string,
  PatientBirthDate: string,
  PatientSex: string,
  PatientAge: string,
  PatientWeight: float,
  PatientSize: float,
  PatientAddress: string,
  PatientMotherBirthName: string,
  StudyInstanceUID: string,
  StudyDate: string,
  StudyTime: string,
  StudyID: string,
  StudyDescription: string,
  SeriesInstanceUID: string,
  SeriesNumber: int,
  SeriesDate: string,
  SeriesTime: string,
  SeriesDescription: string,
  Modality: string,
  SOPClassUID: string,
  SOPInstanceUID: string,
  InstanceNumber: int,
  ImageType: [string],
  AcquisitionDate: string,
  AcquisitionTime: string,
  ContentDate: string,
  ContentTime: string,
  ImagePositionPatient: [float],
  ImageOrientationPatient: [float],
  PixelSpacing: [float],
  SliceThickness: float,
  SpacingBetweenSlices: float,
  Rows: int,
  Columns: int,
  BitsAllocated: int,
  BitsStored: int,
  HighBit: int,
  PixelRepresentation: int,
  PhotometricInterpretation: string,
  SamplesPerPixel: int,
  PlanarConfiguration: int,
  PixelData: bytes,
  WindowCenter: [float],
  WindowWidth: [float],
  RescaleIntercept: float,
  RescaleSlope: float,
  RescaleType: string,
  ReferencedSOPClassUID: string,
  ReferencedSOPInstanceUID: string,
  ReferencedFrameNumber: [int],
  ReferencedSegmentNumber: [int],
  ReferencedImageSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string,
    ReferencedFrameNumber: [int],
    ReferencedSegmentNumber: [int]
  }],
  ReferencedStudySequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedSeriesSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedPatientSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedPerformedProcedureStepSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTPlanSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedStructureSetSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedDoseSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedInstanceSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string,
    ReferencedFrameNumber: [int],
    ReferencedSegmentNumber: [int]
  }],
  ReferencedRealWorldValueMappingSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedWaveformSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedPresentationStateSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedOverlaySequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedCurveSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedModalityLUTSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedVOILUTSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedAnnotationSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedSpatialRegistrationSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedColorPaletteSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedPresentationLUTSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedStereometricSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedSurfaceSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedTractSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedVolumeSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedWaveformAnnotationSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTDoseSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTImageSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTPlanSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTStructureSetSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTTreatmentRecordSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTTreatmentSummarySequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTTreatmentSessionSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTTreatmentPhaseSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }],
  ReferencedRTTreatmentCourseSequence: [{
    ReferencedSOPClassUID: string,
    ReferencedSOPInstanceUID: string
  }]
};