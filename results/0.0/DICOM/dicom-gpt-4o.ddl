namespace DICOM

// Define the DICOM file format
type DICOMFile = struct {
    preamble: bytes(128); // 128-byte preamble
    prefix: "DICM"; // 4-byte DICOM prefix
    elements: list<DICOMElement>; // List of DICOM elements
}

// Define a DICOM element
type DICOMElement = struct {
    tag: DICOMTag; // DICOM tag
    vr: VR; // Value Representation
    length: uint32; // Length of the value
    value: bytes(length); // Value of the element
}

// Define a DICOM tag
type DICOMTag = struct {
    group: uint16; // Group number
    element: uint16; // Element number
}

// Define Value Representation (VR)
enum VR : string {
    "AE", // Application Entity
    "AS", // Age String
    "AT", // Attribute Tag
    "CS", // Code String
    "DA", // Date
    "DS", // Decimal String
    "DT", // DateTime
    "FL", // Floating Point Single
    "FD", // Floating Point Double
    "IS", // Integer String
    "LO", // Long String
    "LT", // Long Text
    "OB", // Other Byte
    "OD", // Other Double
    "OF", // Other Float
    "OL", // Other Long
    "OW", // Other Word
    "PN", // Person Name
    "SH", // Short String
    "SL", // Signed Long
    "SQ", // Sequence of Items
    "SS", // Signed Short
    "ST", // Short Text
    "TM", // Time
    "UC", // Unlimited Characters
    "UI", // Unique Identifier
    "UL", // Unsigned Long
    "UN", // Unknown
    "UR", // URI/URL
    "US", // Unsigned Short
    "UT"  // Unlimited Text
}