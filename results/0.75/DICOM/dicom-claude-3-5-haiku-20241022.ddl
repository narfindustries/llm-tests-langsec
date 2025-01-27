dicom_file = {
    preamble: bytes[128],
    magic_number: "DICM" as bytes[4],
    
    metadata_group: {
        group_length: u32le,
        transfer_syntax: transfer_syntax_item,
        patient_name: patient_name_item,
        patient_id: patient_id_item,
        // Other metadata items as needed
    },
    
    pixel_data_group: {
        group_length: u32le,
        pixel_data: bytes
    }
}

transfer_syntax_item = {
    tag: [0x0002, 0x0010] as bytes[2],
    vr: "UI" as bytes[2],
    length: u16le,
    value: string
}

patient_name_item = {
    tag: [0x0010, 0x0010] as bytes[2], 
    vr: "PN" as bytes[2],
    length: u16le,
    value: string
}

patient_id_item = {
    tag: [0x0010, 0x0020] as bytes[2],
    vr: "LO" as bytes[2], 
    length: u16le,
    value: string
}