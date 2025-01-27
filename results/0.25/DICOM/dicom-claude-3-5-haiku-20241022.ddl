type DICOM_Header = {
    file_preamble: [u8; 128],
    dicom_prefix: [u8; 4],
    group: u16,
    element: u16,
    vr: [u8; 2],
    length: u32
}

type TransferSyntax = 
    | ImplicitVRLittleEndian
    | ExplicitVRLittleEndian
    | ExplicitVRBigEndian

type PixelRepresentation =
    | Unsigned
    | Signed

type PhotometricInterpretation =
    | Monochrome1
    | Monochrome2
    | PaletteColor
    | RGB

type DicomImage = {
    header: DICOM_Header,
    transfer_syntax: TransferSyntax,
    pixel_representation: PixelRepresentation,
    photometric_interpretation: PhotometricInterpretation,
    rows: u16,
    columns: u16,
    bits_allocated: u16,
    bits_stored: u16,
    high_bit: u16,
    pixel_data: [u8]
}

parse dicom_file = 
    header:DICOM_Header -> 
    transfer_syntax:TransferSyntax ->
    pixel_representation:PixelRepresentation ->
    photometric_interpretation:PhotometricInterpretation ->
    rows:u16 ->
    columns:u16 ->
    bits_allocated:u16 ->
    bits_stored:u16 ->
    high_bit:u16 ->
    pixel_data:[u8] ->
    DicomImage {
        header,
        transfer_syntax,
        pixel_representation,
        photometric_interpretation,
        rows,
        columns,
        bits_allocated,
        bits_stored,
        high_bit,
        pixel_data
    }