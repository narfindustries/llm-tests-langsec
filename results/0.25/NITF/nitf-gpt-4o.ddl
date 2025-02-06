NITF: struct {
    FileHeader: struct {
        FHDR: string[9], // File header version
        CLEVEL: uint8, // Complexity level
        STYPE: string[4], // Standard type
        OSTAID: string[10], // Originating station ID
        FDT: string[14], // File date and time
        FTITLE: string[80], // File title
        FSCLAS: string[1], // File security classification
        FSCODE: string[40], // File security code
        FSCTLH: string[40], // File security control and handling
        FSREL: string[40], // File security release instructions
        FSDCTP: string[2], // File security declassification type
        FSDCDT: string[8], // File security declassification date
        FSDCXM: string[4], // File security declassification exemption
        FSDG: string[1], // File security downgrading
        FSDGDT: string[8], // File security downgrading date
        FSCLTX: string[43], // File security classification text
        FSCATP: string[1], // File security classification authority type
        FSCAUT: string[40], // File security classification authority
        FSCRSN: string[1], // File security classification reason
        FSSRDT: string[8], // File security source date
        FSCTLN: string[15] // File security control number
        // Additional fields for file header
    },
    ImageSegment: struct {
        IM: string[2], // Image segment marker
        IID1: string[10], // Image identifier
        IDATIM: string[14], // Image date and time
        TGTID: string[17], // Target identifier
        ISCLAS: string[1], // Image security classification
        ISCODE: string[40], // Image security code
        ISCTLH: string[40], // Image security control and handling
        ISREL: string[40], // Image security release instructions
        ISDCTP: string[2], // Image security declassification type
        ISDCDT: string[8], // Image security declassification date
        ISDCXM: string[4], // Image security declassification exemption
        ISDG: string[1], // Image security downgrading
        ISDGDT: string[8], // Image security downgrading date
        ISCLTX: string[43], // Image security classification text
        ISCATP: string[1], // Image security classification authority type
        ISCAUT: string[40], // Image security classification authority
        ISCRSN: string[1], // Image security classification reason
        ISSRDT: string[8], // Image security source date
        ISCTLN: string[15] // Image security control number
        // Additional fields for image segment
    },
    GraphicSegment: struct {
        GRN: string[2], // Graphic segment number
        GID: string[10], // Graphic identifier
        GDTIM: string[14] // Graphic date and time
        // Additional fields for graphic segment
    },
    TextSegment: struct {
        TXT: string[2], // Text segment marker
        TXTID: string[10], // Text identifier
        TXTDT: string[14] // Text date and time
        // Additional fields for text segment
    },
    DataExtensionSegment: struct {
        DESID: string[25], // Data extension identifier
        DESVER: string[2] // Data extension version
        // Additional fields for data extension segment
    },
    ReservedExtensionSegment: struct {
        // Fields for reserved extension segment
    }
}