Daedalus;

type Text = string : read string;

type FileHeader = struct {
    FHDR : Text(4);     // File Profile Name
    FVER : Text(5);     // File Version
    CLEVEL : Text(2);   // Complexity Level
    STYPE : Text(4);    // Standard Type
    OSTAID : Text(10);  // Originating Station ID
    FTITLE : Text(80);  // File Title
    FSCLAS : Text(1);   // File Security Classification
    FSCOP : Text(5);    // File Copy Number
    FSCPYS : Text(5);   // File Number of Copies
    ENCRYP : Text(1);   // Encryption
    FBKGC : Text(3);    // File Background Color
    ONAME : Text(24);   // Originator Name
    OPHONE : Text(18);  // Originator Phone
    FL : Text(12);      // File Length
    HL : Text(6);       // File Header Length
};

type ImageSubHeader = struct {
    IID1 : Text(10);    // Image Identifier 1
    IDATIM : Text(14);  // Image Date and Time
    TGTID : Text(17);   // Target Identifier
    ITITLE : Text(80);  // Image Title
    ISCLAS : Text(1);   // Security Classification
    ENCRYP : Text(1);   // Encryption
    ISORCE : Text(42);  // Image Source
    NROWS : Text(8);    // Number of Significant Rows in Image
    NCOLS : Text(8);    // Number of Significant Columns in Image
    PVTYPE : Text(3);   // Pixel Value Type
    IREP : Text(8);     // Representation
    ICAT : Text(8);     // Category
    ABPP : Text(2);     // Actual Bits Per Pixel Per Band
    PJUST : Text(1);    // Pixel Justification
    IC : Text(2);       // Compression
    NBANDS : Text(1);   // Number of Bands
    Bands : [Text];     // Dynamic array based on NBANDS
};

type GraphicSubHeader = struct {
    SID : Text(10);     // Graphic Identifier
    SNAME : Text(20);   // Graphic Name
    SCLAS : Text(1);    // Graphic Classification
    ENCRYP : Text(1);   // Graphic Encryption
};

type TextSubHeader = struct {
    TEXTID : Text(7);   // Text Identifier
    TXTDT : Text(14);   // Text Date and Time
    TXTITL : Text(80);  // Text Title
    TSCLAS : Text(1);   // Text Security Classification
};

type DataExtensionSubHeader = struct {
    DESID : Text(25);   // Data Extension ID
    DESVER : Text(2);   // Data Extension Version
    DESCLAS : Text(1);  // Data Extension Classification
};

type ReservedExtensionSubHeader = struct {
    RESID : Text(25);   // Reserved Extension Identifier
    RECLAS : Text(1);   // Reserved Extension Classification
};

type UserDefinedSubHeader = struct {
    UDHD : Text(5);     // User Defined Header Data Length
    UDT : Text;         // User Defined Data
};

type NITF = struct {
    Header : FileHeader;
    ImageSubHeaders : [ImageSubHeader];
    GraphicSubHeaders : [GraphicSubHeader];
    TextSubHeaders : [TextSubHeader];
    DataExtensionSubHeaders : [DataExtensionSubHeader];
    ReservedExtensionSubHeaders : [ReservedExtensionSubHeader];
    UserDefinedSubHeaders : [UserDefinedSubHeader];
};