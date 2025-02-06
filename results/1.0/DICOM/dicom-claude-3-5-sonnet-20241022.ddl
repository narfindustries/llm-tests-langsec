value_type BE;

let DICOM_HEADER = {
    preamble: u8[128];
    magic: "DICM";
};

let VR_TYPES = select {
    "AE" => "Application Entity",
    "AS" => "Age String",
    "AT" => "Attribute Tag",
    "CS" => "Code String",
    "DA" => "Date",
    "DS" => "Decimal String",
    "DT" => "DateTime",
    "FL" => "Floating Point Single",
    "FD" => "Floating Point Double",
    "IS" => "Integer String",
    "LO" => "Long String",
    "LT" => "Long Text",
    "OB" => "Other Byte",
    "OD" => "Other Double",
    "OF" => "Other Float",
    "OW" => "Other Word",
    "PN" => "Person Name",
    "SH" => "Short String",
    "SL" => "Signed Long",
    "SQ" => "Sequence",
    "SS" => "Signed Short",
    "ST" => "Short Text",
    "TM" => "Time",
    "UI" => "Unique Identifier",
    "UL" => "Unsigned Long",
    "UN" => "Unknown",
    "US" => "Unsigned Short",
    "UT" => "Unlimited Text"
};

let DATA_ELEMENT = {
    group: u16;
    element: u16;
    vr: string(2);
    length: if $vr in ["OB", "OW", "OF", "SQ", "UN", "UT"] {
        _: u16;
        value: u32;
    } else {
        value: u16;
    };
    value: select ($vr) {
        "AE" => string($length.value),
        "AS" => string(4),
        "AT" => u32[$length.value/4],
        "CS" => string($length.value),
        "DA" => string(8),
        "DS" => string($length.value),
        "DT" => string($length.value),
        "FL" => f32[$length.value/4],
        "FD" => f64[$length.value/8],
        "IS" => string($length.value),
        "LO" => string($length.value),
        "LT" => string($length.value),
        "OB" => u8[$length.value],
        "OD" => f64[$length.value/8],
        "OF" => f32[$length.value/4],
        "OW" => u16[$length.value/2],
        "PN" => string($length.value),
        "SH" => string($length.value),
        "SL" => i32[$length.value/4],
        "SQ" => SEQUENCE($length.value),
        "SS" => i16[$length.value/2],
        "ST" => string($length.value),
        "TM" => string($length.value),
        "UI" => string($length.value),
        "UL" => u32[$length.value/4],
        "UN" => u8[$length.value],
        "US" => u16[$length.value/2],
        "UT" => string($length.value)
    };
};

let SEQUENCE = function(length: u32) {
    items: ITEM[] with end = start + length;
};

let ITEM = {
    tag: u32;
    length: u32;
    data: if $length == 0xFFFFFFFF {
        elements: DATA_ELEMENT[] until peek u32 == 0xFFFEE00D;
        delimiter: u32;
    } else {
        elements: DATA_ELEMENT[] with end = start + $length;
    };
};

let DICOM = {
    header: DICOM_HEADER;
    elements: DATA_ELEMENT[];
};