type NITF = struct {
    file_header: FileHeader,
    image_segments: [ImageSegment],
    graphic_segments: [GraphicSegment],
    text_segments: [TextSegment],
    data_extension_segments: [DataExtensionSegment],
    reserved_extension_segments: [ReservedExtensionSegment]
}

type FileHeader = struct {
    fhdr: string[9], // File Profile Name
    clevel: string[2], // Compliance Level
    stype: string[4], // System Type
    ostaid: string[10], // Originating Station ID
    fdt: string[14], // File Date and Time
    ftitle: string[80], // File Title
    fsclas: string[1], // File Security Classification
    fscode: string[40], // File Security Classification Code
    fsctlh: string[40], // File Security Control and Handling
    fsrel: string[40], // File Releasing Instructions
    fscaut: string[20], // File Security Classification Authority
    fsctln: string[20], // File Security Control Number
    fsdctp: string[1], // File Security Downgrade
    fsdcdt: string[8], // File Security Downgrade Date
    fsdcxm: string[4], // File Security Downgrade Exemption
    fscom: string[40], // File Security Classification Comment
    fsdg: string[1], // File Security Downgrade
    fsdgt: string[40], // File Security Downgrade Text
    oname: string[27], // Originator's Name
    ophone: string[18] // Originator's Phone Number
}

type ImageSegment = struct {
    im: string[2], // Image Identifier
    iid1: string[10], // Image Identifier 1
    idatim: string[14], // Image Date and Time
    tgtid: string[17], // Target Identifier
    iid2: string[80], // Image Identifier 2
    isclas: string[1], // Image Security Classification
    iscode: string[40], // Image Security Classification Code
    isctlh: string[40], // Image Security Control and Handling
    isrel: string[40], // Image Releasing Instructions
    iscaut: string[20], // Image Security Classification Authority
    isctln: string[20], // Image Security Control Number
    isdctp: string[1], // Image Security Downgrade
    isdcdt: string[8], // Image Security Downgrade Date
    isdcxm: string[4], // Image Security Downgrade Exemption
    isdg: string[1], // Image Security Downgrade
    isdgt: string[40], // Image Security Downgrade Text
    encryp: string[1], // Encryption
    isorce: string[42], // Image Source
    nrows: uint32, // Number of Rows
    ncols: uint32, // Number of Columns
    pvtype: string[3], // Pixel Value Type
    irep: string[8], // Image Representation
    icat: string[8], // Image Category
    abpp: uint16, // Actual Bits Per Pixel
    pjust: string[1], // Pixel Justification
    icoords: string[1], // Image Coordinate System
    igeolo: string[60] // Image Geographic Location
}

type GraphicSegment = struct {
    loff: uint32, // Line Offset
    coff: uint32, // Column Offset
    sloc: string[10] // Segment Location
}

type TextSegment = struct {
    textid: string[7], // Text Identifier
    txtdt: string[14], // Text Date and Time
    txtitl: string[80] // Text Title
}

type DataExtensionSegment = struct {
    desid: string[25], // Data Extension Identifier
    desver: string[2] // Data Extension Version
}

type ReservedExtensionSegment = struct {
    // Reserved for future use
}