type dicom = {
    preamble: [128]u8,
    magic_number: [4]u8,
    meta_header: struct {
        group_length: u32,
        transfer_syntax_uid: str,
        implementation_uid: str,
        version_name: optional str
    },
    patient: struct {
        name: optional str,
        id: optional str,
        birth_date: optional str,
        sex: optional enum {
            Male,
            Female,
            Other
        },
        age: optional str,
        weight: optional f32
    },
    study: struct {
        instance_uid: str,
        date: optional str,
        time: optional str,
        accession_number: optional str,
        referring_physician: optional str,
        description: optional str
    },
    series: struct {
        modality: enum {
            CT,
            MR,
            US,
            XA,
            CR,
            DR,
            RF
        },
        instance_uid: str,
        number: optional u32,
        description: optional str
    },
    image: struct {
        sop_instance_uid: str,
        instance_number: optional u32,
        position: optional [3]f32,
        orientation: optional [6]f32,
        pixel_spacing: optional [2]f32,
        slice_thickness: optional f32
    },
    pixel_data: struct {
        rows: u16,
        columns: u16,
        bits_allocated: u16,
        bits_stored: u16,
        pixel_representation: enum {
            Unsigned,
            Signed
        },
        samples_per_pixel: u16,
        photometric_interpretation: enum {
            Monochrome1,
            Monochrome2,
            RGB
        },
        data: []u8
    }
}