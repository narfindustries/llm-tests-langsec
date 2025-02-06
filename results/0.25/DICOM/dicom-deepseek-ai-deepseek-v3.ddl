DICOMSpecification = {
    header: DICOMHeader,
    data_elements: [DICOMDataElement]
}

DICOMHeader = {
    preamble: Preamble,
    prefix: Prefix,
    group_length: GroupLength
}

Preamble = bytes(128)

Prefix = bytes(4)

GroupLength = uint32

DICOMDataElement = {
    tag: Tag,
    vr: VR,
    value_length: ValueLength,
    value: Value
}

Tag = {
    group: uint16,
    element: uint16
}

VR = string(2)

ValueLength = uint32

Value = bytes(ValueLength)

OptionalDICOMDataElement = optional DICOMDataElement

DICOMIOD = {
    patient_module: PatientModule,
    study_module: StudyModule,
    series_module: SeriesModule,
    image_module: ImageModule,
    optional_modules: [OptionalModule]
}

PatientModule = {
    patient_name: DICOMDataElement,
    patient_id: DICOMDataElement,
    patient_birth_date: OptionalDICOMDataElement,
    patient_sex: OptionalDICOMDataElement
}

StudyModule = {
    study_date: DICOMDataElement,
    study_time: DICOMDataElement,
    study_id: DICOMDataElement,
    study_instance_uid: DICOMDataElement,
    referring_physician_name: OptionalDICOMDataElement
}

SeriesModule = {
    modality: DICOMDataElement,
    series_instance_uid: DICOMDataElement,
    series_number: OptionalDICOMDataElement,
    body_part_examined: OptionalDICOMDataElement
}

ImageModule = {
    image_type: DICOMDataElement,
    samples_per_pixel: DICOMDataElement,
    photometric_interpretation: DICOMDataElement,
    rows: DICOMDataElement,
    columns: DICOMDataElement,
    bits_allocated: DICOMDataElement,
    bits_stored: DICOMDataElement,
    high_bit: DICOMDataElement,
    pixel_representation: DICOMDataElement,
    pixel_data: DICOMDataElement
}

OptionalModule = {
    sop_class_uid: OptionalDICOMDataElement,
    sop_instance_uid: OptionalDICOMDataElement,
    transfer_syntax_uid: OptionalDICOMDataElement,
    implementation_class_uid: OptionalDICOMDataElement,
    implementation_version_name: OptionalDICOMDataElement
}

DICOMServiceClass = {
    sop_class_uid: DICOMDataElement,
    sop_instance_uid: DICOMDataElement,
    service_class_user: ServiceClassUser,
    service_class_provider: ServiceClassProvider
}

ServiceClassUser = {
    scu_role: DICOMDataElement,
    scu_ae_title: OptionalDICOMDataElement
}

ServiceClassProvider = {
    scp_role: DICOMDataElement,
    scp_ae_title: OptionalDICOMDataElement
}

DICOMTransferSyntax = {
    transfer_syntax_uid: DICOMDataElement,
    byte_order: ByteOrder,
    compression: Compression
}

ByteOrder = enum {
    LittleEndian,
    BigEndian
}

Compression = enum {
    None,
    JPEG,
    JPEGLossless,
    JPEG2000,
    RLE
}

DICOMDataDictionary = {
    data_elements: [DICOMDataElement]
}

DICOMConformance = {
    implementation_class_uid: DICOMDataElement,
    implementation_version_name: DICOMDataElement,
    conformance_statement: DICOMDataElement
}

DICOMSecurity = {
    digital_signatures: OptionalDICOMDataElement,
    encryption: OptionalDICOMDataElement,
    user_authentication: OptionalDICOMDataElement
}

DICOMWebAccess = {
    wado_url: DICOMDataElement,
    wado_rs_url: OptionalDICOMDataElement,
    wado_uri: OptionalDICOMDataElement
}

DICOMApplicationHosting = {
    application_hosting_profile: DICOMDataElement,
    application_hosting_capabilities: OptionalDICOMDataElement
}

DICOMImagingReports = {
    report_type: DICOMDataElement,
    report_content: DICOMDataElement
}

DICOMSpecification = {
    header: DICOMHeader,
    data_elements: [DICOMDataElement],
    iods: [DICOMIOD],
    service_classes: [DICOMServiceClass],
    transfer_syntaxes: [DICOMTransferSyntax],
    data_dictionary: DICOMDataDictionary,
    conformance: DICOMConformance,
    security: DICOMSecurity,
    web_access: DICOMWebAccess,
    application_hosting: DICOMApplicationHosting,
    imaging_reports: DICOMImagingReports
}