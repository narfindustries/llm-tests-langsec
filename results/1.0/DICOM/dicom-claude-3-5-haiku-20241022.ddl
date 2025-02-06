type Dicom = {
    header: {
        preamble: Array<Byte, 128>,
        magic_number: Array<Byte, 4>,
        transfer_syntax: Enum<TransferSyntax> {
            ImplicitVRLittleEndian,
            ExplicitVRLittleEndian, 
            ExplicitVRBigEndian
        },
        implementation_version: String
    },
    patient: {
        name: String,
        id: String,
        birth_date: Date,
        sex: Enum<Sex> {
            Male,
            Female,
            Other
        },
        age: Optional<UInt8>,
        weight: Optional<Float32>,
        height: Optional<Float32>
    },
    study: {
        instance_uid: UUID,
        date: Date,
        time: Time,
        accession_number: Optional<String>,
        referring_physician: Optional<String>, 
        description: Optional<String>,
        study_id: Optional<String>
    },
    series: {
        modality: Enum<Modality> {
            CT, MR, US, NM, PET,
            CR, DX, MG, RF, XA, RG
        },
        series_number: UInt16,
        description: Optional<String>,
        date: Date,
        time: Time
    },
    image: {
        sop_class_uid: UUID,
        image_type: Enum<ImageType> {
            Original,
            Derived,
            Secondary
        },
        instance_number: UInt32,
        pixel_data: Array<Byte>
    }
}