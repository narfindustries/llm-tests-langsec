grammar NITF;

import std.core;

struct FileHeader {
    char FHDR[9]; // File Header and Version
    char CLEVEL[2]; // Complexity Level
    char STYPE[4]; // Standard Type
    char OSTAID[10]; // Originating Station ID
    char FDT[14]; // File Date and Time
    char FTITLE[80]; // File Title
    char FSCLAS[1]; // Security Classification
    char FSCLSY[2]; // Security Classification System
    char FSCODE[11]; // Codewords
    char FSCTLH[2]; // Control and Handling
    char FSREL[20]; // Release Instructions
    char FSDCTP[2]; // Declassification Type
    char FSDCDT[8]; // Declassification Date
    char FSDCXM[4]; // Declassification Exemption
    char FSDG[1]; // Downgrade
    char FSDGDT[8]; // Downgrade Date
    char FSCLTX[43]; // Classification Text
    char FSCATP[1]; // Classification Authority Type
    char FSCAUT[40]; // Classification Authority
    char FSCRSN[1]; // Classification Reason
    char FSSRDT[8]; // Security Source Date
    char FSCTLN[15]; // Security Control Number
    char FSCOP[5]; // Copy Number
    char FSCPYS[5]; // Number of Copies
    char ENCRYP[1]; // Encryption
    char FBKGC[24]; // Background Color
    char ONAME[27]; // Originator's Name
    char OPHONE[18]; // Originator's Phone Number
}

struct ImageSubheader {
    char IID1[10]; // Image Identifier 1
    char IDATIM[14]; // Image Date and Time
    char TGTID[17]; // Target Identifier
    char IID2[80]; // Image Identifier 2
    char ISCLAS[1]; // Image Security Classification
    char ISCLSY[2]; // Image Classification System
    char ISCODE[11]; // Image Codewords
    char ISCTLH[2]; // Image Control and Handling
    char ISREL[20]; // Image Release Instructions
    char ISDCTP[2]; // Image Declassification Type
    char ISDCDT[8]; // Image Declassification Date
    char ISDCXM[4]; // Image Declassification Exemption
    char ISDG[1]; // Image Downgrade
    char ISDGDT[8]; // Image Downgrade Date
    char ISCLTX[43]; // Image Classification Text
    char ISCATP[1]; // Image Classification Authority Type
    char ISCAUT[40]; // Image Classification Authority
    char ISCRSN[1]; // Image Classification Reason
    char ISSRDT[8]; // Image Security Source Date
    char ISCTLN[15]; // Image Security Control Number
    char ISORCE[42]; // Image Source
    u16 NROWS; // Number of Rows
    u16 NCOLS; // Number of Columns
    char PVTYPE[3]; // Pixel Value Type
    char IREP[8]; // Image Representation
    char ICAT[8]; // Image Category
    char ABPP[2]; // Actual Bits Per Pixel
    char PJUST[1]; // Pixel Justification
    char ICORDS[1]; // Image Coordinate System
    char IGEOLO[60]; // Image Geolocation
    u8 NICOM; // Number of Image Comments
    char ICOM[80]; // Image Comment
    char IC[2]; // Image Compression
    u8 NBANDS; // Number of Bands
    char XBANDS[5]; // Extended Number of Bands (if NBANDS > 9)
}

struct NITF {
    FileHeader fileHeader;
    ImageSubheader imageSubheader;
}