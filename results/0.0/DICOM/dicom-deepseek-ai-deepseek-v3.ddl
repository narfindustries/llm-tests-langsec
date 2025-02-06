type DICOMSpecification = {
    Header: {
        Preamble: Bytes[128],
        Prefix: "DICM"
    },
    DataSet: {
        Element: {
            Tag: UInt16,
            VR: String[2],
            Length: UInt16,
            Value: Bytes[Length]
        }*
    },
    Modules: {
        PatientModule: {
            PatientName: Element(Tag: 0x00100010, VR: "PN"),
            PatientID: Element(Tag: 0x00100020, VR: "LO"),
            PatientBirthDate: Element(Tag: 0x00100030, VR: "DA")?,
            PatientSex: Element(Tag: 0x00100040, VR: "CS")?
        },
        StudyModule: {
            StudyInstanceUID: Element(Tag: 0x0020000D, VR: "UI"),
            StudyDate: Element(Tag: 0x00080020, VR: "DA")?,
            StudyTime: Element(Tag: 0x00080030, VR: "TM")?,
            ReferringPhysicianName: Element(Tag: 0x00080090, VR: "PN")?,
            StudyID: Element(Tag: 0x00200010, VR: "SH")?,
            AccessionNumber: Element(Tag: 0x00080050, VR: "SH")?
        },
        SeriesModule: {
            SeriesInstanceUID: Element(Tag: 0x0020000E, VR: "UI"),
            Modality: Element(Tag: 0x00080060, VR: "CS"),
            SeriesNumber: Element(Tag: 0x00200011, VR: "IS")?,
            SeriesDate: Element(Tag: 0x00080021, VR: "DA")?,
            SeriesTime: Element(Tag: 0x00080031, VR: "TM")?,
            ProtocolName: Element(Tag: 0x00181030, VR: "LO")?
        },
        ImageModule: {
            SOPInstanceUID: Element(Tag: 0x00080018, VR: "UI"),
            ImageType: Element(Tag: 0x00080008, VR: "CS"),
            InstanceNumber: Element(Tag: 0x00200013, VR: "IS")?,
            AcquisitionDate: Element(Tag: 0x00080022, VR: "DA")?,
            AcquisitionTime: Element(Tag: 0x00080032, VR: "TM")?,
            SamplesPerPixel: Element(Tag: 0x00280002, VR: "US"),
            PhotometricInterpretation: Element(Tag: 0x00280004, VR: "CS"),
            Rows: Element(Tag: 0x00280010, VR: "US"),
            Columns: Element(Tag: 0x00280011, VR: "US"),
            BitsAllocated: Element(Tag: 0x00280100, VR: "US"),
            BitsStored: Element(Tag: 0x00280101, VR: "US"),
            HighBit: Element(Tag: 0x00280102, VR: "US"),
            PixelRepresentation: Element(Tag: 0x00280103, VR: "US"),
            PixelData: Element(Tag: 0x7FE00010, VR: "OB" | "OW")
        },
        OptionalModules: {
            ClinicalTrialSeriesModule: {
                ClinicalTrialCoordinatingCenterName: Element(Tag: 0x00120050, VR: "LO")?,
                ClinicalTrialSeriesID: Element(Tag: 0x00120051, VR: "LO")?,
                ClinicalTrialSeriesDescription: Element(Tag: 0x00120060, VR: "LO")?
            },
            FrameOfReferenceModule: {
                FrameOfReferenceUID: Element(Tag: 0x00200052, VR: "UI"),
                PositionReferenceIndicator: Element(Tag: 0x00200040, VR: "LO")?
            },
            SOPCommonModule: {
                SOPClassUID: Element(Tag: 0x00080016, VR: "UI"),
                SOPInstanceUID: Element(Tag: 0x00080018, VR: "UI"),
                SpecificCharacterSet: Element(Tag: 0x00080005, VR: "CS")?,
                InstanceCreationDate: Element(Tag: 0x00080012, VR: "DA")?,
                InstanceCreationTime: Element(Tag: 0x00080013, VR: "TM")?,
                InstanceCreatorUID: Element(Tag: 0x00080014, VR: "UI")?
            }
        }
    }
}