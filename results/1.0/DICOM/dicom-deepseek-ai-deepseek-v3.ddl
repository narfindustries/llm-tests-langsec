DICOMFile = {
    header: Header,
    data: Data
}

Header = {
    preamble: Preamble,
    prefix: Prefix,
    elements: Elements
}

Preamble = uint8[128]

Prefix = uint32 where . == 0x2a4449434f4d

Elements = Element*

Element = {
    group: Group,
    element: Element,
    vr: VR,
    length: Length,
    value: Value
}

Group = uint16

Element = uint16

VR = uint8[2]

Length = uint16

Value = {
    case vr of
        "AE" -> ApplicationEntityValue,
        "AS" -> AgeStringValue,
        "AT" -> AttributeTagValue,
        "CS" -> CodeStringValue,
        "DA" -> DateValue,
        "DS" -> DecimalStringValue,
        "DT" -> DateTimeValue,
        "FD" -> FloatingPointDoubleValue,
        "FL" -> FloatingPointSingleValue,
        "IS" -> IntegerStringValue,
        "LO" -> LongStringValue,
        "LT" -> LongTextValue,
        "OB" -> OtherByteValue,
        "OD" -> OtherDoubleValue,
        "OF" -> OtherFloatValue,
        "OL" -> OtherLongValue,
        "OW" -> OtherWordValue,
        "PN" -> PersonNameValue,
        "SH" -> ShortStringValue,
        "SL" -> SignedLongValue,
        "SQ" -> SequenceValue,
        "SS" -> SignedShortValue,
        "ST" -> ShortTextValue,
        "TM" -> TimeValue,
        "UI" -> UniqueIdentifierValue,
        "UL" -> UnsignedLongValue,
        "US" -> UnsignedShortValue,
        "UT" -> UnlimitedTextValue
}

ApplicationEntityValue = uint8[length]

AgeStringValue = uint8[length]

AttributeTagValue = {
    group: uint16,
    element: uint16
}

CodeStringValue = uint8[length]

DateValue = uint8[length]

DecimalStringValue = uint8[length]

DateTimeValue = uint8[length]

FloatingPointDoubleValue = double

FloatingPointSingleValue = float

IntegerStringValue = uint8[length]

LongStringValue = uint8[length]

LongTextValue = uint8[length]

OtherByteValue = uint8[length]

OtherDoubleValue = double[length/8]

OtherFloatValue = float[length/4]

OtherLongValue = uint32[length/4]

OtherWordValue = uint16[length/2]

PersonNameValue = uint8[length]

ShortStringValue = uint8[length]

SignedLongValue = int32

SequenceValue = {
    length: uint32,
    items: Elements
}

SignedShortValue = int16

ShortTextValue = uint8[length]

TimeValue = uint8[length]

UniqueIdentifierValue = uint8[length]

UnsignedLongValue = uint32

UnsignedShortValue = uint16

UnlimitedTextValue = uint8[length]

Data = uint8*

DICOMTags = {
    (0x0002, 0x0001) => "FileMetaInformationVersion",
    (0x0002, 0x0010) => "TransferSyntaxUID",
    (0x0008, 0x0008) => "ImageType",
    (0x0008, 0x0016) => "SOPClassUID",
    (0x0008, 0x0020) => "StudyDate",
    (0x0008, 0x0030) => "StudyTime",
    (0x0008, 0x0060) => "Modality",
    (0x0010, 0x0010) => "PatientsName",
    (0x0010, 0x0020) => "PatientID",
    (0x0010, 0x0030) => "PatientsBirthDate",
    (0x0010, 0x0040) => "PatientsSex",
    (0x0020, 0x000D) => "StudyInstanceUID",
    (0x0020, 0x0013) => "InstanceNumber",
    (0x0020, 0x0020) => "PatientOrientation",
    (0x7FE0, 0x0010) => "PixelData"
}

VRs = {
    "AE", "AS", "AT", "CS", "DA", "DS", "DT", "FD", "FL", "IS", "LO", "LT", "OB", "OD", "OF", "OL", "OW", "PN", "SH", "SL", "SQ", "SS", "ST", "TM", "UI", "UL", "US", "UT"
}